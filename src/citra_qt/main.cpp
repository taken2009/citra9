// Copyright 2014 Citra Emulator Project
// Licensed under GPLv2 or any later version
// Refer to the license.txt file included.

#include <cinttypes>
#include <clocale>
#include <memory>
#include <thread>
#include <glad/glad.h>
#define QT_NO_OPENGL
#include <QDesktopWidget>
#include <QFileDialog>
#include <QFutureWatcher>
#include <QMessageBox>
#include <QtConcurrent/QtConcurrentRun>
#include <QtGui>
#include <QtWidgets>
#include "citra_qt/aboutdialog.h"
#include "citra_qt/bootmanager.h"
#include "citra_qt/compatdb.h"
#include "citra_qt/cheat_gui.h"
#include "citra_qt/configuration/config.h"
#include "citra_qt/configuration/configure_dialog.h"
#include "citra_qt/debugger/graphics/graphics.h"
#include "citra_qt/debugger/graphics/graphics_breakpoints.h"
#include "citra_qt/debugger/graphics/graphics_cmdlists.h"
#include "citra_qt/debugger/graphics/graphics_surface.h"
#include "citra_qt/debugger/graphics/graphics_tracing.h"
#include "citra_qt/debugger/graphics/graphics_vertex_shader.h"
#include "citra_qt/debugger/profiler.h"
#include "citra_qt/debugger/registers.h"
#include "citra_qt/debugger/wait_tree.h"
#include "citra_qt/game_list.h"
#include "citra_qt/hotkeys.h"
#include "citra_qt/main.h"
#include "citra_qt/ui_settings.h"
#include "citra_qt/updater/updater.h"
#include "common/logging/backend.h"
#include "common/logging/filter.h"
#include "common/logging/log.h"
#include "common/logging/text_formatter.h"
#include "common/microprofile.h"
#include "common/platform.h"
#include "common/scm_rev.h"
#include "common/scope_exit.h"
#include "common/string_util.h"
#include "core/core.h"
#include "core/file_sys/archive_source_sd_savedata.h"
#include "core/gdbstub/gdbstub.h"
#include "core/loader/loader.h"
#include "core/settings.h"

#ifdef QT_STATICPLUGIN
Q_IMPORT_PLUGIN(QWindowsIntegrationPlugin);
#endif

/**
 * "Callouts" are one-time instructional messages shown to the user. In the config settings, there
 * is a bitfield "callout_flags" options, used to track if a message has already been shown to the
 * user. This is 32-bits - if we have more than 32 callouts, we should retire and recyle old ones.
 */
enum class CalloutFlag : uint32_t {
    Telemetry = 0x1,
};

static void ShowCalloutMessage(const QString& message, CalloutFlag flag) {
    if (UISettings::values.callout_flags & static_cast<uint32_t>(flag)) {
        return;
    }

    UISettings::values.callout_flags |= static_cast<uint32_t>(flag);

    QMessageBox msg;
    msg.setText(message);
    msg.setStandardButtons(QMessageBox::Ok);
    msg.setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);
    msg.setStyleSheet("QLabel{min-width: 900px;}");
    msg.exec();
}

void GMainWindow::ShowCallouts() {
    static const QString telemetry_message =
        tr("To help improve Citra, the Citra Team collects anonymous usage data. No private or "
           "personally identifying information is collected. This data helps us to understand how "
           "people use Citra and prioritize our efforts. Furthermore, it helps us to more easily "
           "identify emulation bugs and performance issues. This data includes:<ul><li>Information"
           " about the version of Citra you are using</li><li>Performance data about the games you "
           "play</li><li>Your configuration settings</li><li>Information about your computer "
           "hardware</li><li>Emulation errors and crash information</li></ul>By default, this "
           "feature is enabled. To disable this feature, click 'Emulation' from the menu and then "
           "select 'Configure...'. Then, on the 'Web' tab, uncheck 'Share anonymous usage data with"
           " the Citra team'. <br/><br/>By using this software, you agree to the above terms.<br/>"
           "<br/><a href='https://citra-emu.org/entry/telemetry-and-why-thats-a-good-thing/'>Learn "
           "more</a>");
    ShowCalloutMessage(telemetry_message, CalloutFlag::Telemetry);
}

GMainWindow::GMainWindow() : config(new Config()), emu_thread(nullptr) {
    // register size_t to use in slots and signals
    qRegisterMetaType<size_t>("size_t");

    LoadTranslation();

    Pica::g_debug_context = Pica::DebugContext::Construct();
    setAcceptDrops(true);
    ui.setupUi(this);
    statusBar()->hide();

    InitializeWidgets();
    InitializeDebugWidgets();
    InitializeRecentFileMenuActions();
    InitializeHotkeys();
    ShowUpdaterWidgets();

    SetDefaultUIGeometry();
    RestoreUIState();

    ConnectMenuEvents();
    ConnectWidgetEvents();

    SetupUIStrings();

    show();

    game_list->PopulateAsync(UISettings::values.gamedir, UISettings::values.gamedir_deepscan);

    UpdateUITheme();

    // Show one-time "callout" messages to the user
    ShowCallouts();

    if (UISettings::values.check_for_update_on_start) {
        CheckForUpdates();
    }

    QStringList args = QApplication::arguments();
    if (args.length() >= 2) {
        BootGame(args[1]);
    }
}

GMainWindow::~GMainWindow() {
    // will get automatically deleted otherwise
    if (render_window->parent() == nullptr)
        delete render_window;

    Pica::g_debug_context.reset();
}

void GMainWindow::InitializeWidgets() {
#ifdef CITRA_ENABLE_COMPATIBILITY_REPORTING
    ui.action_Report_Compatibility->setVisible(true);
#endif
    render_window = new GRenderWindow(this, emu_thread.get());
    render_window->hide();

    game_list = new GameList(this);
    ui.horizontalLayout->addWidget(game_list);

    // Setup updater
    updater = new Updater(this);
    UISettings::values.updater_found = updater->HasUpdater();

    // Create status bar
    message_label = new QLabel();
    // Configured separately for left alignment
    message_label->setVisible(false);
    message_label->setFrameStyle(QFrame::NoFrame);
    message_label->setContentsMargins(4, 0, 4, 0);
    message_label->setAlignment(Qt::AlignLeft);
    statusBar()->addPermanentWidget(message_label, 1);

    progress_bar = new QProgressBar();
    progress_bar->hide();
    statusBar()->addPermanentWidget(progress_bar);

    emu_speed_label = new QLabel();
    emu_speed_label->setToolTip(tr("Current emulation speed. Values higher or lower than 100% "
                                   "indicate emulation is running faster or slower than a 3DS."));
    game_fps_label = new QLabel();
    game_fps_label->setToolTip(tr("How many frames per second the game is currently displaying. "
                                  "This will vary from game to game and scene to scene."));
    emu_frametime_label = new QLabel();
    emu_frametime_label->setToolTip(
        tr("Time taken to emulate a 3DS frame, not counting framelimiting or v-sync. For "
           "full-speed emulation this should be at most 16.67 ms."));

    for (auto& label : {emu_speed_label, game_fps_label, emu_frametime_label}) {
        label->setVisible(false);
        label->setFrameStyle(QFrame::NoFrame);
        label->setContentsMargins(4, 0, 4, 0);
        statusBar()->addPermanentWidget(label, 0);
    }
    statusBar()->setVisible(true);
    setStyleSheet("QStatusBar::item{border: none;}");
}

void GMainWindow::InitializeDebugWidgets() {
    connect(ui.action_Create_Pica_Surface_Viewer, &QAction::triggered, this,
            &GMainWindow::OnCreateGraphicsSurfaceViewer);

    QMenu* debug_menu = ui.menu_View_Debugging;

#if MICROPROFILE_ENABLED
    microProfileDialog = new MicroProfileDialog(this);
    microProfileDialog->hide();
    debug_menu->addAction(microProfileDialog->toggleViewAction());
#endif

    registersWidget = new RegistersWidget(this);
    addDockWidget(Qt::RightDockWidgetArea, registersWidget);
    registersWidget->hide();
    debug_menu->addAction(registersWidget->toggleViewAction());
    connect(this, &GMainWindow::EmulationStarting, registersWidget,
            &RegistersWidget::OnEmulationStarting);
    connect(this, &GMainWindow::EmulationStopping, registersWidget,
            &RegistersWidget::OnEmulationStopping);

    graphicsWidget = new GPUCommandStreamWidget(this);
    addDockWidget(Qt::RightDockWidgetArea, graphicsWidget);
    graphicsWidget->hide();
    debug_menu->addAction(graphicsWidget->toggleViewAction());

    graphicsCommandsWidget = new GPUCommandListWidget(this);
    addDockWidget(Qt::RightDockWidgetArea, graphicsCommandsWidget);
    graphicsCommandsWidget->hide();
    debug_menu->addAction(graphicsCommandsWidget->toggleViewAction());

    graphicsBreakpointsWidget = new GraphicsBreakPointsWidget(Pica::g_debug_context, this);
    addDockWidget(Qt::RightDockWidgetArea, graphicsBreakpointsWidget);
    graphicsBreakpointsWidget->hide();
    debug_menu->addAction(graphicsBreakpointsWidget->toggleViewAction());

    graphicsVertexShaderWidget = new GraphicsVertexShaderWidget(Pica::g_debug_context, this);
    addDockWidget(Qt::RightDockWidgetArea, graphicsVertexShaderWidget);
    graphicsVertexShaderWidget->hide();
    debug_menu->addAction(graphicsVertexShaderWidget->toggleViewAction());

    graphicsTracingWidget = new GraphicsTracingWidget(Pica::g_debug_context, this);
    addDockWidget(Qt::RightDockWidgetArea, graphicsTracingWidget);
    graphicsTracingWidget->hide();
    debug_menu->addAction(graphicsTracingWidget->toggleViewAction());
    connect(this, &GMainWindow::EmulationStarting, graphicsTracingWidget,
            &GraphicsTracingWidget::OnEmulationStarting);
    connect(this, &GMainWindow::EmulationStopping, graphicsTracingWidget,
            &GraphicsTracingWidget::OnEmulationStopping);

    waitTreeWidget = new WaitTreeWidget(this);
    addDockWidget(Qt::LeftDockWidgetArea, waitTreeWidget);
    waitTreeWidget->hide();
    debug_menu->addAction(waitTreeWidget->toggleViewAction());
    connect(this, &GMainWindow::EmulationStarting, waitTreeWidget,
            &WaitTreeWidget::OnEmulationStarting);
    connect(this, &GMainWindow::EmulationStopping, waitTreeWidget,
            &WaitTreeWidget::OnEmulationStopping);
}

void GMainWindow::InitializeRecentFileMenuActions() {
    for (int i = 0; i < max_recent_files_item; ++i) {
        actions_recent_files[i] = new QAction(this);
        actions_recent_files[i]->setVisible(false);
        connect(actions_recent_files[i], &QAction::triggered, this, &GMainWindow::OnMenuRecentFile);

        ui.menu_recent_files->addAction(actions_recent_files[i]);
    }

    UpdateRecentFiles();
}

void GMainWindow::InitializeHotkeys() {
    RegisterHotkey("Main Window", "Load File", QKeySequence::Open);
    RegisterHotkey("Main Window", "Swap Screens", QKeySequence::NextChild);
    RegisterHotkey("Main Window", "Start Emulation");
    RegisterHotkey("Main Window", "Fullscreen", QKeySequence::FullScreen);
    RegisterHotkey("Main Window", "Exit Fullscreen", QKeySequence(Qt::Key_Escape),
                   Qt::ApplicationShortcut);
    RegisterHotkey("Main Window", "Increase Speed Limit", QKeySequence("+"),
                   Qt::ApplicationShortcut);
    RegisterHotkey("Main Window", "Decrease Speed Limit", QKeySequence("-"),
                   Qt::ApplicationShortcut);
    LoadHotkeys();

    connect(GetHotkey("Main Window", "Load File", this), &QShortcut::activated, this,
            &GMainWindow::OnMenuLoadFile);
    connect(GetHotkey("Main Window", "Start Emulation", this), &QShortcut::activated, this,
            &GMainWindow::OnStartGame);
    connect(GetHotkey("Main Window", "Swap Screens", render_window), &QShortcut::activated, this,
            &GMainWindow::OnSwapScreens);
    connect(GetHotkey("Main Window", "Fullscreen", render_window), &QShortcut::activated,
            ui.action_Fullscreen, &QAction::trigger);
    connect(GetHotkey("Main Window", "Fullscreen", render_window), &QShortcut::activatedAmbiguously,
            ui.action_Fullscreen, &QAction::trigger);
    connect(GetHotkey("Main Window", "Exit Fullscreen", this), &QShortcut::activated, this, [&] {
        if (emulation_running) {
            ui.action_Fullscreen->setChecked(false);
            ToggleFullscreen();
        }
    });
    constexpr u16 SPEED_LIMIT_STEP = 5;
    connect(GetHotkey("Main Window", "Increase Speed Limit", this), &QShortcut::activated, this,
            [&] {
                if (Settings::values.frame_limit < 9999 - SPEED_LIMIT_STEP) {
                    Settings::values.frame_limit += SPEED_LIMIT_STEP;
                    UpdateStatusBar();
                }
            });
    connect(GetHotkey("Main Window", "Decrease Speed Limit", this), &QShortcut::activated, this,
            [&] {
                if (Settings::values.frame_limit > SPEED_LIMIT_STEP) {
                    Settings::values.frame_limit -= SPEED_LIMIT_STEP;
                    UpdateStatusBar();
                }
            });
}

void GMainWindow::ShowUpdaterWidgets() {
    ui.action_Check_For_Updates->setVisible(UISettings::values.updater_found);
    ui.action_Open_Maintenance_Tool->setVisible(UISettings::values.updater_found);

    connect(updater, &Updater::CheckUpdatesDone, this, &GMainWindow::OnUpdateFound);
}

void GMainWindow::SetDefaultUIGeometry() {
    // geometry: 55% of the window contents are in the upper screen half, 45% in the lower half
    const QRect screenRect = QApplication::desktop()->screenGeometry(this);

    const int w = screenRect.width() * 2 / 3;
    const int h = screenRect.height() / 2;
    const int x = (screenRect.x() + screenRect.width()) / 2 - w / 2;
    const int y = (screenRect.y() + screenRect.height()) / 2 - h * 55 / 100;

    setGeometry(x, y, w, h);
}

void GMainWindow::RestoreUIState() {
    restoreGeometry(UISettings::values.geometry);
    restoreState(UISettings::values.state);
    render_window->restoreGeometry(UISettings::values.renderwindow_geometry);
#if MICROPROFILE_ENABLED
    microProfileDialog->restoreGeometry(UISettings::values.microprofile_geometry);
    microProfileDialog->setVisible(UISettings::values.microprofile_visible);
#endif

    ui.action_Cheats->setEnabled(false);
    game_list->LoadInterfaceLayout();

    ui.action_Single_Window_Mode->setChecked(UISettings::values.single_window_mode);
    ToggleWindowMode();

    ui.action_Fullscreen->setChecked(UISettings::values.fullscreen);

    ui.action_Display_Dock_Widget_Headers->setChecked(UISettings::values.display_titlebar);
    OnDisplayTitleBars(ui.action_Display_Dock_Widget_Headers->isChecked());

    ui.action_Show_Filter_Bar->setChecked(UISettings::values.show_filter_bar);
    game_list->setFilterVisible(ui.action_Show_Filter_Bar->isChecked());

    ui.action_Show_Status_Bar->setChecked(UISettings::values.show_status_bar);
    statusBar()->setVisible(ui.action_Show_Status_Bar->isChecked());
}

void GMainWindow::ConnectWidgetEvents() {
    connect(game_list, &GameList::GameChosen, this, &GMainWindow::OnGameListLoadFile);
    connect(game_list, &GameList::OpenSaveFolderRequested, this,
            &GMainWindow::OnGameListOpenSaveFolder);

    connect(this, &GMainWindow::EmulationStarting, render_window,
            &GRenderWindow::OnEmulationStarting);
    connect(this, &GMainWindow::EmulationStopping, render_window,
            &GRenderWindow::OnEmulationStopping);

    connect(&status_bar_update_timer, &QTimer::timeout, this, &GMainWindow::UpdateStatusBar);

    connect(this, &GMainWindow::UpdateProgress, this, &GMainWindow::OnUpdateProgress);
}

void GMainWindow::ConnectMenuEvents() {
    // File
    connect(ui.action_Load_File, &QAction::triggered, this, &GMainWindow::OnMenuLoadFile);
    connect(ui.action_Install_CIA, &QAction::triggered, this, &GMainWindow::OnMenuInstallCIA);
    connect(ui.action_Select_Game_List_Root, &QAction::triggered, this,
            &GMainWindow::OnMenuSelectGameListRoot);
    connect(ui.action_Exit, &QAction::triggered, this, &QMainWindow::close);

    // Emulation
    connect(ui.action_Start, &QAction::triggered, this, &GMainWindow::OnStartGame);
    connect(ui.action_Pause, &QAction::triggered, this, &GMainWindow::OnPauseGame);
    connect(ui.action_Stop, &QAction::triggered, this, &GMainWindow::OnStopGame);
    connect(ui.action_Report_Compatibility, &QAction::triggered, this,
            &GMainWindow::OnMenuReportCompatibility);
    connect(ui.action_Configure, &QAction::triggered, this, &GMainWindow::OnConfigure);
    connect(ui.action_Cheats, SIGNAL(triggered()), this, SLOT(OnCheats()));

    // View
    connect(ui.action_Single_Window_Mode, &QAction::triggered, this,
            &GMainWindow::ToggleWindowMode);
    connect(ui.action_Display_Dock_Widget_Headers, &QAction::triggered, this,
            &GMainWindow::OnDisplayTitleBars);
    ui.action_Show_Filter_Bar->setShortcut(tr("CTRL+F"));
    connect(ui.action_Show_Filter_Bar, &QAction::triggered, this, &GMainWindow::OnToggleFilterBar);
    connect(ui.action_Show_Status_Bar, &QAction::triggered, statusBar(), &QStatusBar::setVisible);
    ui.action_Fullscreen->setShortcut(GetHotkey("Main Window", "Fullscreen", this)->key());
    connect(ui.action_Fullscreen, &QAction::triggered, this, &GMainWindow::ToggleFullscreen);

    // Help
    connect(ui.action_FAQ, &QAction::triggered,
            []() { QDesktopServices::openUrl(QUrl("https://citra-emu.org/wiki/faq/")); });
    connect(ui.action_About, &QAction::triggered, this, &GMainWindow::OnMenuAboutCitra);
    connect(ui.action_Check_For_Updates, &QAction::triggered, this,
            &GMainWindow::OnCheckForUpdates);
    connect(ui.action_Open_Maintenance_Tool, &QAction::triggered, this,
            &GMainWindow::OnOpenUpdater);
}

void GMainWindow::OnDisplayTitleBars(bool show) {
    QList<QDockWidget*> widgets = findChildren<QDockWidget*>();

    if (show) {
        for (QDockWidget* widget : widgets) {
            QWidget* old = widget->titleBarWidget();
            widget->setTitleBarWidget(nullptr);
            if (old != nullptr)
                delete old;
        }
    } else {
        for (QDockWidget* widget : widgets) {
            QWidget* old = widget->titleBarWidget();
            widget->setTitleBarWidget(new QWidget());
            if (old != nullptr)
                delete old;
        }
    }
}

void GMainWindow::OnCheckForUpdates() {
    explicit_update_check = true;
    CheckForUpdates();
}

void GMainWindow::CheckForUpdates() {
    if (updater->CheckForUpdates()) {
        LOG_INFO(Frontend, "Update check started");
    } else {
        LOG_WARNING(Frontend, "Unable to start check for updates");
    }
}

void GMainWindow::OnUpdateFound(bool found, bool error) {
    if (error) {
        LOG_WARNING(Frontend, "Update check failed");
        return;
    }

    if (!found) {
        LOG_INFO(Frontend, "No updates found");

        // If the user explicitly clicked the "Check for Updates" button, we are
        //  going to want to show them a prompt anyway.
        if (explicit_update_check) {
            explicit_update_check = false;
            ShowNoUpdatePrompt();
        }
        return;
    }

    if (emulation_running && !explicit_update_check) {
        LOG_INFO(Frontend, "Update found, deferring as game is running");
        defer_update_prompt = true;
        return;
    }

    LOG_INFO(Frontend, "Update found!");
    explicit_update_check = false;

    ShowUpdatePrompt();
}

void GMainWindow::ShowUpdatePrompt() {
    defer_update_prompt = false;

    auto result = QMessageBox::question(
        this, tr("Update available!"),
        tr("An update for Citra is available. Do you wish to install it now?<br /><br />"
           "This <b>will</b> terminate emulation, if it is running."),
        QMessageBox::Yes | QMessageBox::No, QMessageBox::Yes);

    if (result == QMessageBox::Yes) {
        updater->LaunchUIOnExit();
        close();
    }
}

void GMainWindow::ShowNoUpdatePrompt() {
    QMessageBox::information(this, tr("No update found"), tr("No update has been found for Citra."),
                             QMessageBox::Ok, QMessageBox::Ok);
}

void GMainWindow::OnOpenUpdater() {
    updater->LaunchUI();
}

bool GMainWindow::LoadROM(const QString& filename) {
    // Shutdown previous session if the emu thread is still active...
    if (emu_thread != nullptr)
        ShutdownGame();

    render_window->InitRenderTarget();
    render_window->MakeCurrent();

    if (!gladLoadGL()) {
        QMessageBox::critical(this, tr("Error while initializing OpenGL 3.3 Core!"),
                              tr("Your GPU may not support OpenGL 3.3, or you do not "
                                 "have the latest graphics driver."));
        return false;
    }

    Core::System& system{Core::System::GetInstance()};

    const Core::System::ResultStatus result{system.Load(render_window, filename.toStdString())};

    if (result != Core::System::ResultStatus::Success) {
        switch (result) {
        case Core::System::ResultStatus::ErrorGetLoader:
            LOG_CRITICAL(Frontend, "Failed to obtain loader for %s!",
                         filename.toStdString().c_str());
            QMessageBox::critical(this, tr("Error while loading ROM!"),
                                  tr("The ROM format is not supported."));
            break;

        case Core::System::ResultStatus::ErrorSystemMode:
            LOG_CRITICAL(Frontend, "Failed to load ROM!");
            QMessageBox::critical(this, tr("Error while loading ROM!"),
                                  tr("Could not determine the system mode."));
            break;

        case Core::System::ResultStatus::ErrorLoader_ErrorEncrypted: {
            QMessageBox::critical(
                this, tr("Error while loading ROM!"),
                tr("The game that you are trying to load must be decrypted before being used with "
                   "Citra. A real 3DS is required.<br/><br/>"
                   "For more information on dumping and decrypting games, please see the following "
                   "wiki pages: <ul>"
                   "<li><a href='https://citra-emu.org/wiki/dumping-game-cartridges/'>Dumping Game "
                   "Cartridges</a></li>"
                   "<li><a href='https://citra-emu.org/wiki/dumping-installed-titles/'>Dumping "
                   "Installed Titles</a></li>"
                   "</ul>"));
            break;
        }
        case Core::System::ResultStatus::ErrorLoader_ErrorInvalidFormat:
            QMessageBox::critical(this, tr("Error while loading ROM!"),
                                  tr("The ROM format is not supported."));
            break;

        case Core::System::ResultStatus::ErrorVideoCore:
            QMessageBox::critical(
                this, tr("An error occured in the video core."),
                tr("Citra has encountered an error while running the video core, please see the "
                   "log for more details."
                   "For more information on accessing the log, please see the following page: "
                   "<a href='https://community.citra-emu.org/t/how-to-upload-the-log-file/296'>How "
                   "to "
                   "Upload the Log File</a>."
                   "Ensure that you have the latest graphics drivers for your GPU."));

            break;

        default:
            QMessageBox::critical(
                this, tr("Error while loading ROM!"),
                tr("An unknown error occured. Please see the log for more details."));
            break;
        }
        return false;
    }

    Core::Telemetry().AddField(Telemetry::FieldType::App, "Frontend", "Qt");
    return true;
}

void GMainWindow::BootGame(const QString& filename) {
    LOG_INFO(Frontend, "Citra starting...");
    StoreRecentFile(filename); // Put the filename on top of the list

    if (!LoadROM(filename))
        return;

    // Create and start the emulation thread
    emu_thread = std::make_unique<EmuThread>(render_window);
    emit EmulationStarting(emu_thread.get());
    render_window->moveContext();
    emu_thread->start();

    connect(render_window, &GRenderWindow::Closed, this, &GMainWindow::OnStopGame);
    // BlockingQueuedConnection is important here, it makes sure we've finished refreshing our views
    // before the CPU continues
    connect(emu_thread.get(), &EmuThread::DebugModeEntered, registersWidget,
            &RegistersWidget::OnDebugModeEntered, Qt::BlockingQueuedConnection);
    connect(emu_thread.get(), &EmuThread::DebugModeEntered, waitTreeWidget,
            &WaitTreeWidget::OnDebugModeEntered, Qt::BlockingQueuedConnection);
    connect(emu_thread.get(), &EmuThread::DebugModeLeft, registersWidget,
            &RegistersWidget::OnDebugModeLeft, Qt::BlockingQueuedConnection);
    connect(emu_thread.get(), &EmuThread::DebugModeLeft, waitTreeWidget,
            &WaitTreeWidget::OnDebugModeLeft, Qt::BlockingQueuedConnection);

    // Update the GUI
    registersWidget->OnDebugModeEntered();
    if (ui.action_Single_Window_Mode->isChecked()) {
        game_list->hide();
    }
    status_bar_update_timer.start(2000);

    render_window->show();
    render_window->setFocus();

    emulation_running = true;
    if (ui.action_Fullscreen->isChecked()) {
        ShowFullscreen();
    }
    OnStartGame();
}

void GMainWindow::ShutdownGame() {
    emu_thread->RequestStop();

    // Release emu threads from any breakpoints
    // This belongs after RequestStop() and before wait() because if emulation stops on a GPU
    // breakpoint after (or before) RequestStop() is called, the emulation would never be able
    // to continue out to the main loop and terminate. Thus wait() would hang forever.
    // TODO(bunnei): This function is not thread safe, but it's being used as if it were
    Pica::g_debug_context->ClearBreakpoints();

    emit EmulationStopping();

    // Wait for emulation thread to complete and delete it
    emu_thread->wait();
    emu_thread = nullptr;

    // The emulation is stopped, so closing the window or not does not matter anymore
    disconnect(render_window, &GRenderWindow::Closed, this, &GMainWindow::OnStopGame);

    // Update the GUI
    ui.action_Cheats->setEnabled(false);
    ui.action_Start->setEnabled(false);
    ui.action_Start->setText(tr("Start"));
    ui.action_Pause->setEnabled(false);
    ui.action_Stop->setEnabled(false);
    ui.action_Report_Compatibility->setEnabled(false);
    render_window->hide();
    game_list->show();
    game_list->setFilterFocus();

    // Disable status bar updates
    status_bar_update_timer.stop();
    message_label->setVisible(false);
    emu_speed_label->setVisible(false);
    game_fps_label->setVisible(false);
    emu_frametime_label->setVisible(false);

    emulation_running = false;

    if (defer_update_prompt) {
        ShowUpdatePrompt();
    }
}

void GMainWindow::StoreRecentFile(const QString& filename) {
    UISettings::values.recent_files.prepend(filename);
    UISettings::values.recent_files.removeDuplicates();
    while (UISettings::values.recent_files.size() > max_recent_files_item) {
        UISettings::values.recent_files.removeLast();
    }

    UpdateRecentFiles();
}

void GMainWindow::UpdateRecentFiles() {
    unsigned int num_recent_files =
        std::min(UISettings::values.recent_files.size(), static_cast<int>(max_recent_files_item));

    for (unsigned int i = 0; i < num_recent_files; i++) {
        QString text = QString("&%1. %2").arg(i + 1).arg(
            QFileInfo(UISettings::values.recent_files[i]).fileName());
        actions_recent_files[i]->setText(text);
        actions_recent_files[i]->setData(UISettings::values.recent_files[i]);
        actions_recent_files[i]->setToolTip(UISettings::values.recent_files[i]);
        actions_recent_files[i]->setVisible(true);
    }

    for (int j = num_recent_files; j < max_recent_files_item; ++j) {
        actions_recent_files[j]->setVisible(false);
    }

    // Grey out the recent files menu if the list is empty
    if (num_recent_files == 0) {
        ui.menu_recent_files->setEnabled(false);
    } else {
        ui.menu_recent_files->setEnabled(true);
    }
}

void GMainWindow::OnGameListLoadFile(QString game_path) {
    BootGame(game_path);
}

void GMainWindow::OnGameListOpenSaveFolder(u64 program_id) {
    std::string sdmc_dir = FileUtil::GetUserPath(D_SDMC_IDX);
    std::string path = FileSys::ArchiveSource_SDSaveData::GetSaveDataPathFor(sdmc_dir, program_id);
    QString qpath = QString::fromStdString(path);

    QDir dir(qpath);
    if (!dir.exists()) {
        QMessageBox::critical(this, tr("Error Opening Save Folder"), tr("Folder does not exist!"));
        return;
    }

    LOG_INFO(Frontend, "Opening save data path for program_id=%" PRIu64, program_id);
    QDesktopServices::openUrl(QUrl::fromLocalFile(qpath));
}

void GMainWindow::OnMenuLoadFile() {
    QString extensions;
    for (const auto& piece : game_list->supported_file_extensions)
        extensions += "*." + piece + " ";

    QString file_filter = tr("3DS Executable") + " (" + extensions + ")";
    file_filter += ";;" + tr("All Files (*.*)");

    QString filename = QFileDialog::getOpenFileName(this, tr("Load File"),
                                                    UISettings::values.roms_path, file_filter);
    if (!filename.isEmpty()) {
        UISettings::values.roms_path = QFileInfo(filename).path();

        BootGame(filename);
    }
}

void GMainWindow::OnMenuSelectGameListRoot() {
    QString dir_path = QFileDialog::getExistingDirectory(this, tr("Select Directory"));
    if (!dir_path.isEmpty()) {
        UISettings::values.gamedir = dir_path;
        game_list->PopulateAsync(dir_path, UISettings::values.gamedir_deepscan);
    }
}

void GMainWindow::OnMenuInstallCIA() {
    QString filepath = QFileDialog::getOpenFileName(
        this, tr("Load File"), UISettings::values.roms_path,
        tr("3DS Installation File (*.CIA*)") + ";;" + tr("All Files (*.*)"));
    if (filepath.isEmpty())
        return;

    ui.action_Install_CIA->setEnabled(false);
    progress_bar->show();
    watcher = new QFutureWatcher<Service::AM::InstallStatus>;
    QFuture<Service::AM::InstallStatus> f = QtConcurrent::run([&, filepath] {
        const auto cia_progress = [&](size_t written, size_t total) {
            emit UpdateProgress(written, total);
        };
        return Service::AM::InstallCIA(filepath.toStdString(), cia_progress);
    });
    connect(watcher, &QFutureWatcher<Service::AM::InstallStatus>::finished, this,
            &GMainWindow::OnCIAInstallFinished);
    watcher->setFuture(f);
}

void GMainWindow::OnUpdateProgress(size_t written, size_t total) {
    progress_bar->setMaximum(total);
    progress_bar->setValue(written);
}

void GMainWindow::OnCIAInstallFinished() {
    progress_bar->hide();
    progress_bar->setValue(0);
    switch (watcher->future()) {
    case Service::AM::InstallStatus::Success:
        this->statusBar()->showMessage(tr("The file has been installed successfully."));
        break;
    case Service::AM::InstallStatus::ErrorFailedToOpenFile:
        QMessageBox::critical(this, tr("Unable to open File"),
                              tr("Could not open the selected file"));
        break;
    case Service::AM::InstallStatus::ErrorAborted:
        QMessageBox::critical(
            this, tr("Installation aborted"),
            tr("The installation was aborted. Please see the log for more details"));
        break;
    case Service::AM::InstallStatus::ErrorInvalid:
        QMessageBox::critical(this, tr("Invalid File"), tr("The selected file is not a valid CIA"));
        break;
    case Service::AM::InstallStatus::ErrorEncrypted:
        QMessageBox::critical(this, tr("Encrypted File"),
                              tr("The file that you are trying to install must be decrypted "
                                 "before being used with Citra. A real 3DS is required."));
        break;
    }
    delete watcher;
    ui.action_Install_CIA->setEnabled(true);
}

void GMainWindow::OnMenuRecentFile() {
    QAction* action = qobject_cast<QAction*>(sender());
    assert(action);

    QString filename = action->data().toString();
    QFileInfo file_info(filename);
    if (file_info.exists()) {
        BootGame(filename);
    } else {
        // Display an error message and remove the file from the list.
        QMessageBox::information(this, tr("File not found"),
                                 tr("File \"%1\" not found").arg(filename));

        UISettings::values.recent_files.removeOne(filename);
        UpdateRecentFiles();
    }
}

void GMainWindow::OnStartGame() {
    emu_thread->SetRunning(true);
    qRegisterMetaType<Core::System::ResultStatus>("Core::System::ResultStatus");
    qRegisterMetaType<std::string>("std::string");
    connect(emu_thread.get(), &EmuThread::ErrorThrown, this, &GMainWindow::OnCoreError);

    ui.action_Start->setEnabled(false);
    ui.action_Start->setText(tr("Continue"));
    ui.action_Cheats->setEnabled(true);

    ui.action_Pause->setEnabled(true);
    ui.action_Stop->setEnabled(true);
    ui.action_Report_Compatibility->setEnabled(true);
}

void GMainWindow::OnPauseGame() {
    emu_thread->SetRunning(false);

    ui.action_Start->setEnabled(true);
    ui.action_Pause->setEnabled(false);
    ui.action_Stop->setEnabled(true);
}

void GMainWindow::OnStopGame() {
    ShutdownGame();
}

void GMainWindow::OnMenuReportCompatibility() {
    if (!Settings::values.citra_token.empty() && !Settings::values.citra_username.empty()) {
        CompatDB compatdb{this};
        compatdb.exec();
    } else {
        QMessageBox::critical(
            this, tr("Missing Citra Account"),
            tr("In order to submit a game compatibility test case, you must link your Citra "
               "account.<br><br/>To link your Citra account, go to Emulation \> Configuration \> "
               "Web."));
    }
}

void GMainWindow::ToggleFullscreen() {
    if (!emulation_running) {
        return;
    }
    if (ui.action_Fullscreen->isChecked()) {
        ShowFullscreen();
    } else {
        HideFullscreen();
    }
}

void GMainWindow::ShowFullscreen() {
    if (ui.action_Single_Window_Mode->isChecked()) {
        UISettings::values.geometry = saveGeometry();
        ui.menubar->hide();
        statusBar()->hide();
        showFullScreen();
    } else {
        UISettings::values.renderwindow_geometry = render_window->saveGeometry();
        render_window->showFullScreen();
    }
}

void GMainWindow::HideFullscreen() {
    if (ui.action_Single_Window_Mode->isChecked()) {
        statusBar()->setVisible(ui.action_Show_Status_Bar->isChecked());
        ui.menubar->show();
        showNormal();
        restoreGeometry(UISettings::values.geometry);
    } else {
        render_window->showNormal();
        render_window->restoreGeometry(UISettings::values.renderwindow_geometry);
    }
}

void GMainWindow::ToggleWindowMode() {
    if (ui.action_Single_Window_Mode->isChecked()) {
        // Render in the main window...
        render_window->BackupGeometry();
        ui.horizontalLayout->addWidget(render_window);
        render_window->setFocusPolicy(Qt::ClickFocus);
        if (emulation_running) {
            render_window->setVisible(true);
            render_window->setFocus();
            game_list->hide();
        }

    } else {
        // Render in a separate window...
        ui.horizontalLayout->removeWidget(render_window);
        render_window->setParent(nullptr);
        render_window->setFocusPolicy(Qt::NoFocus);
        if (emulation_running) {
            render_window->setVisible(true);
            render_window->RestoreGeometry();
            game_list->show();
        }
    }
}

void GMainWindow::OnConfigure() {
    ConfigureDialog configureDialog(this);
    connect(&configureDialog, &ConfigureDialog::languageChanged, this,
            &GMainWindow::OnLanguageChanged);
    auto result = configureDialog.exec();
    if (result == QDialog::Accepted) {
        configureDialog.applyConfiguration();
        UpdateUITheme();
        config->Save();
    }
}

void GMainWindow::OnToggleFilterBar() {
    game_list->setFilterVisible(ui.action_Show_Filter_Bar->isChecked());
    if (ui.action_Show_Filter_Bar->isChecked()) {
        game_list->setFilterFocus();
    } else {
        game_list->clearFilter();
    }
}

void GMainWindow::OnSwapScreens() {
    Settings::values.swap_screen = !Settings::values.swap_screen;
    Settings::Apply();
}

void GMainWindow::OnCheats() {
    if (cheatWindow == nullptr)
    {
        cheatWindow = std::make_shared<CheatDialog>(this);
    }
    cheatWindow->show();
}

void GMainWindow::OnCreateGraphicsSurfaceViewer() {
    auto graphicsSurfaceViewerWidget = new GraphicsSurfaceWidget(Pica::g_debug_context, this);
    addDockWidget(Qt::RightDockWidgetArea, graphicsSurfaceViewerWidget);
    // TODO: Maybe graphicsSurfaceViewerWidget->setFloating(true);
    graphicsSurfaceViewerWidget->show();
}

void GMainWindow::UpdateStatusBar() {
    if (emu_thread == nullptr) {
        status_bar_update_timer.stop();
        return;
    }

    auto results = Core::System::GetInstance().GetAndResetPerfStats();

    if (Settings::values.use_frame_limit) {
        emu_speed_label->setText(tr("Speed: %1% / %2%")
                                    .arg(results.emulation_speed * 100.0, 0, 'f', 0)
                                    .arg(Settings::values.frame_limit));
    } else {
        emu_speed_label->setText(tr("Speed: %1%")
                                    .arg(results.emulation_speed * 100.0, 0, 'f', 0));
    }
    game_fps_label->setText(tr("Game: %1 FPS").arg(results.game_fps, 0, 'f', 0));
    emu_frametime_label->setText(tr("Frame: %1 ms").arg(results.frametime * 1000.0, 0, 'f', 2));

    emu_speed_label->setVisible(true);
    game_fps_label->setVisible(true);
    emu_frametime_label->setVisible(true);
}

void GMainWindow::OnCoreError(Core::System::ResultStatus result, std::string details) {
    QMessageBox::StandardButton answer;
    QString status_message;
    const QString common_message =
        tr("The game you are trying to load requires additional files from your 3DS to be dumped "
           "before playing.<br/><br/>For more information on dumping these files, please see the "
           "following wiki page: <a "
           "href='https://citra-emu.org/wiki/"
           "dumping-system-archives-and-the-shared-fonts-from-a-3ds-console/'>Dumping System "
           "Archives and the Shared Fonts from a 3DS Console</a>.<br/><br/>Would you like to quit "
           "back to the game list? Continuing emulation may result in crashes, corrupted save "
           "data, or other bugs.");
    switch (result) {
    case Core::System::ResultStatus::ErrorSystemFiles: {
        QString message = "Citra was unable to locate a 3DS system archive";
        if (!details.empty()) {
            message.append(tr(": %1. ").arg(details.c_str()));
        } else {
            message.append(". ");
        }
        message.append(common_message);

        answer = QMessageBox::question(this, tr("System Archive Not Found"), message,
                                       QMessageBox::Yes | QMessageBox::No, QMessageBox::No);
        status_message = "System Archive Missing";
        break;
    }

    case Core::System::ResultStatus::ErrorSharedFont: {
        QString message = tr("Citra was unable to locate the 3DS shared fonts. ");
        message.append(common_message);
        answer = QMessageBox::question(this, tr("Shared Fonts Not Found"), message,
                                       QMessageBox::Yes | QMessageBox::No, QMessageBox::No);
        status_message = "Shared Font Missing";
        break;
    }

    default:
        answer = QMessageBox::question(
            this, tr("Fatal Error"),
            tr("Citra has encountered a fatal error, please see the log for more details. "
               "For more information on accessing the log, please see the following page: "
               "<a href='https://community.citra-emu.org/t/how-to-upload-the-log-file/296'>How to "
               "Upload the Log File</a>.<br/><br/>Would you like to quit back to the game list? "
               "Continuing emulation may result in crashes, corrupted save data, or other bugs."),
            QMessageBox::Yes | QMessageBox::No, QMessageBox::No);
        status_message = "Fatal Error encountered";
        break;
    }

    if (answer == QMessageBox::Yes) {
        if (emu_thread) {
            ShutdownGame();
        }
    } else {
        // Only show the message if the game is still running.
        if (emu_thread) {
            emu_thread->SetRunning(true);
            message_label->setText(status_message);
            message_label->setVisible(true);
        }
    }
}

void GMainWindow::OnMenuAboutCitra() {
    AboutDialog about{this};
    about.exec();
}

bool GMainWindow::ConfirmClose() {
    if (emu_thread == nullptr || !UISettings::values.confirm_before_closing)
        return true;

    QMessageBox::StandardButton answer =
        QMessageBox::question(this, tr("Citra"), tr("Are you sure you want to close Citra?"),
                              QMessageBox::Yes | QMessageBox::No, QMessageBox::No);
    return answer != QMessageBox::No;
}

void GMainWindow::closeEvent(QCloseEvent* event) {
    if (!ConfirmClose()) {
        event->ignore();
        return;
    }

    UISettings::values.geometry = saveGeometry();
    UISettings::values.state = saveState();
    UISettings::values.renderwindow_geometry = render_window->saveGeometry();
#if MICROPROFILE_ENABLED
    UISettings::values.microprofile_geometry = microProfileDialog->saveGeometry();
    UISettings::values.microprofile_visible = microProfileDialog->isVisible();
#endif
    UISettings::values.single_window_mode = ui.action_Single_Window_Mode->isChecked();
    UISettings::values.fullscreen = ui.action_Fullscreen->isChecked();
    UISettings::values.display_titlebar = ui.action_Display_Dock_Widget_Headers->isChecked();
    UISettings::values.show_filter_bar = ui.action_Show_Filter_Bar->isChecked();
    UISettings::values.show_status_bar = ui.action_Show_Status_Bar->isChecked();
    UISettings::values.first_start = false;

    game_list->SaveInterfaceLayout();
    SaveHotkeys();

    // Shutdown session if the emu thread is active...
    if (emu_thread != nullptr)
        ShutdownGame();

    render_window->close();

    QWidget::closeEvent(event);
}

static bool IsSingleFileDropEvent(QDropEvent* event) {
    const QMimeData* mimeData = event->mimeData();
    return mimeData->hasUrls() && mimeData->urls().length() == 1;
}

void GMainWindow::dropEvent(QDropEvent* event) {
    if (IsSingleFileDropEvent(event) && ConfirmChangeGame()) {
        const QMimeData* mimeData = event->mimeData();
        QString filename = mimeData->urls().at(0).toLocalFile();
        BootGame(filename);
    }
}

void GMainWindow::dragEnterEvent(QDragEnterEvent* event) {
    if (IsSingleFileDropEvent(event)) {
        event->acceptProposedAction();
    }
}

void GMainWindow::dragMoveEvent(QDragMoveEvent* event) {
    event->acceptProposedAction();
}

bool GMainWindow::ConfirmChangeGame() {
    if (emu_thread == nullptr)
        return true;

    auto answer = QMessageBox::question(
        this, tr("Citra"),
        tr("Are you sure you want to stop the emulation? Any unsaved progress will be lost."),
        QMessageBox::Yes | QMessageBox::No, QMessageBox::No);
    return answer != QMessageBox::No;
}

void GMainWindow::filterBarSetChecked(bool state) {
    ui.action_Show_Filter_Bar->setChecked(state);
    emit(OnToggleFilterBar());
}

void GMainWindow::UpdateUITheme() {
    if (UISettings::values.theme != UISettings::themes[0].second) {
        QString theme_uri(":" + UISettings::values.theme + "/style.qss");
        QFile f(theme_uri);
        if (!f.exists()) {
            LOG_ERROR(Frontend, "Unable to set style, stylesheet file not found");
        } else {
            f.open(QFile::ReadOnly | QFile::Text);
            QTextStream ts(&f);
            qApp->setStyleSheet(ts.readAll());
            GMainWindow::setStyleSheet(ts.readAll());
        }
    } else {
        qApp->setStyleSheet("");
        GMainWindow::setStyleSheet("");
    }
}

void GMainWindow::LoadTranslation() {
    // If the selected language is English, no need to install any translation
    if (UISettings::values.language == "en") {
        return;
    }

    bool loaded;

    if (UISettings::values.language.isEmpty()) {
        // If the selected language is empty, use system locale
        loaded = translator.load(QLocale(), "", "", ":/languages/");
    } else {
        // Otherwise load from the specified file
        loaded = translator.load(UISettings::values.language, ":/languages/");
    }

    if (loaded) {
        qApp->installTranslator(&translator);
    } else {
        UISettings::values.language = "en";
    }
}

void GMainWindow::OnLanguageChanged(const QString& locale) {
    if (UISettings::values.language != "en") {
        qApp->removeTranslator(&translator);
    }

    UISettings::values.language = locale;
    LoadTranslation();
    ui.retranslateUi(this);
    SetupUIStrings();
}

void GMainWindow::SetupUIStrings() {
    setWindowTitle(
        tr("Citra %1| %2-%3").arg(Common::g_build_name, Common::g_scm_branch, Common::g_scm_desc));
}

#ifdef main
#undef main
#endif

int main(int argc, char* argv[]) {
    Log::Filter log_filter(Log::Level::Info);
    Log::SetFilter(&log_filter);

    MicroProfileOnThreadCreate("Frontend");
    SCOPE_EXIT({ MicroProfileShutdown(); });

    // Init settings params
    QCoreApplication::setOrganizationName("Citra team");
    QCoreApplication::setApplicationName("Citra");

    QApplication::setAttribute(Qt::AA_X11InitThreads);
    QApplication app(argc, argv);

    // Qt changes the locale and causes issues in float conversion using std::to_string() when
    // generating shaders
    setlocale(LC_ALL, "C");

    GMainWindow main_window;
    // After settings have been loaded by GMainWindow, apply the filter
    log_filter.ParseFilterString(Settings::values.log_filter);

    main_window.show();
    return app.exec();
}

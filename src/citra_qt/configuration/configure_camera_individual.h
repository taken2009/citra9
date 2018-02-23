#pragma once

#include <memory>
#include <QWidget>

namespace Ui {
class ConfigureCameraIndividual;
}

class ConfigureCameraIndividual : public QWidget {
    Q_OBJECT

public:
    explicit ConfigureCameraIndividual(QWidget* parent = nullptr);
    ~ConfigureCameraIndividual();

    void applyConfiguration();
    void retranslateUi();

    void setCameraId(int cameraId);
    void setCameraTitle(QString cameraTitle);

public slots:
    void UpdateCameraMode(int camera_mode_index);
    void OnBrowseImage();

private:
    void setConfiguration();
    int m_CameraId;

    std::unique_ptr<Ui::ConfigureCameraIndividual> ui;
};

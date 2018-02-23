// Copyright 2016 Citra Emulator Project
// Licensed under GPLv2 or any later version
// Refer to the license.txt file included.

#include "citra_qt/configuration/configure_camera.h"
#include "core/settings.h"
#include "ui_configure_camera.h"

ConfigureCamera::ConfigureCamera(QWidget* parent) : QWidget(parent), ui(new Ui::ConfigureCamera) {

    ui->setupUi(this);
    using namespace Service::CAM;
    ui->camera_outer_right->setCameraId(OuterRightCamera);
    ui->camera_outer_left->setCameraId(OuterLeftCamera);
    ui->camera_inner->setCameraId(InnerCamera);

    this->setConfiguration();
}

ConfigureCamera::~ConfigureCamera() {}

void ConfigureCamera::setConfiguration() {}

void ConfigureCamera::applyConfiguration() {
    ui->camera_outer_right->applyConfiguration();
    ui->camera_outer_left->applyConfiguration();
    ui->camera_inner->applyConfiguration();
    Settings::Apply();
}

void ConfigureCamera::retranslateUi() {
    ui->retranslateUi(this);
    ui->camera_outer_right->retranslateUi();
    ui->camera_outer_left->retranslateUi();
    ui->camera_inner->retranslateUi();
}

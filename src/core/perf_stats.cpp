// Copyright 2017 Citra Emulator Project
// Licensed under GPLv2 or any later version
// Refer to the license.txt file included.

#include <chrono>
#include <mutex>
#include <thread>
#include "common/math_util.h"
#include "core/hw/gpu.h"
#include "core/perf_stats.h"
#include "core/settings.h"

using namespace std::chrono_literals;
using DoubleSecs = std::chrono::duration<double, std::chrono::seconds::period>;
using std::chrono::duration_cast;
using std::chrono::microseconds;

namespace Core {

void PerfStats::BeginSystemFrame() {
    std::lock_guard<std::mutex> lock(object_mutex);

    frame_begin = Clock::now();
}

void PerfStats::EndSystemFrame() {
    std::lock_guard<std::mutex> lock(object_mutex);

    auto frame_end = Clock::now();
    accumulated_frametime += frame_end - frame_begin;
    system_frames += 1;

    previous_frame_length = frame_end - previous_frame_end;
    previous_frame_end = frame_end;
}

void PerfStats::EndGameFrame() {
    std::lock_guard<std::mutex> lock(object_mutex);

    game_frames += 1;
}

PerfStats::Results PerfStats::GetAndResetStats(u64 current_system_time_us) {
    std::lock_guard<std::mutex> lock(object_mutex);

    auto now = Clock::now();
    // Walltime elapsed since stats were reset
    auto interval = duration_cast<DoubleSecs>(now - reset_point).count();

    auto system_us_per_second =
        static_cast<double>(current_system_time_us - reset_point_system_us) / interval;

    Results results{};
    results.system_fps = static_cast<double>(system_frames) / interval;
    results.game_fps = static_cast<double>(game_frames) / interval;
    results.frametime = duration_cast<DoubleSecs>(accumulated_frametime).count() /
                        static_cast<double>(system_frames);
    results.emulation_speed = system_us_per_second / 1'000'000.0;

    // Reset counters
    reset_point = now;
    reset_point_system_us = current_system_time_us;
    accumulated_frametime = Clock::duration::zero();
    system_frames = 0;
    game_frames = 0;

    return results;
}

double PerfStats::GetLastFrameTimeScale() {
    std::lock_guard<std::mutex> lock(object_mutex);

    constexpr double FRAME_LENGTH = 1.0 / GPU::SCREEN_REFRESH_RATE;
    return duration_cast<DoubleSecs>(previous_frame_length).count() / FRAME_LENGTH;
}

void FrameLimiter::DoFrameLimiting(u64 current_system_time_us) {
    if (!Settings::values.use_frame_limit || Settings::values.frame_limit == 0) {
        return;
    }

    auto now = Clock::now();
    double sleep_scale = Settings::values.frame_limit / 100.0;

    // Max lag caused by slow frames. Shouldn't be more than the length of a frame at the current
    // speed percent or it will clamp too much and prevent this from properly limiting to that
    // percent. High values means it'll take longer after a slow frame to recover and start limiting
    const microseconds max_lag_time_us = duration_cast<microseconds>(
        std::chrono::duration<double, std::chrono::microseconds::period>(25ms / sleep_scale));
    frame_limiting_delta_err += duration_cast<microseconds>(
        std::chrono::duration<double, std::chrono::microseconds::period>(
            (current_system_time_us - previous_system_time_us) / sleep_scale));
    frame_limiting_delta_err -= duration_cast<microseconds>(now - previous_walltime);
    frame_limiting_delta_err =
        MathUtil::Clamp(frame_limiting_delta_err, -max_lag_time_us, max_lag_time_us);

    if (frame_limiting_delta_err > microseconds::zero()) {
        std::this_thread::sleep_for(frame_limiting_delta_err);
        auto now_after_sleep = Clock::now();
        frame_limiting_delta_err -= duration_cast<microseconds>(now_after_sleep - now);
        now = now_after_sleep;
    }

    previous_system_time_us = current_system_time_us;
    previous_walltime = now;
}

} // namespace Core

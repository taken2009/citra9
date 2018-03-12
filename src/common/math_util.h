// Copyright 2013 Dolphin Emulator Project / 2014 Citra Emulator Project
// Licensed under GPLv2 or any later version
// Refer to the license.txt file included.

#pragma once

#include <algorithm>
#include <cstdlib>
#include <type_traits>

namespace MathUtil {

static constexpr float PI = 3.14159265f;

inline bool IntervalsIntersect(unsigned start0, unsigned length0, unsigned start1,
                               unsigned length1) {
    return (std::max(start0, start1) < std::min(start0 + length0, start1 + length1));
}

template <typename T>
inline T Clamp(const T val, const T& min, const T& max) {
    return std::max(min, std::min(max, val));
}

template <class T>
struct Rectangle {
    T left;
    T top;
    T right;
    T bottom;

    Rectangle() {}

    Rectangle(T left, T top, T right, T bottom)
        : left(left), top(top), right(right), bottom(bottom) {}

    T GetWidth() const {
        return std::abs(static_cast<typename std::make_signed<T>::type>(right - left));
    }
    T GetHeight() const {
        return std::abs(static_cast<typename std::make_signed<T>::type>(bottom - top));
    }
    Rectangle<T> TranslateX(const T x) const {
        return Rectangle{left + x, top, right + x, bottom};
    }
    Rectangle<T> TranslateY(const T y) const {
        return Rectangle{left, top + y, right, bottom + y};
    }
    Rectangle<T> Scale(const float s) const {
        return Rectangle{left, top, static_cast<T>(left + GetWidth() * s),
                         static_cast<T>(top + GetHeight() * s)};
    }
};

} // namespace MathUtil

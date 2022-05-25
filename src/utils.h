# pragma once

#include <algorithm>
#include <vector>

// https://stackoverflow.com/questions/1719070/what-is-the-right-approach-when-using-stl-container-for-median-calculation/1719155#1719155

double median (std::vector <int> &v);

double median (std::vector <int> &v)
{
    if (v.size () == 0)
        return -1.0;

    int n = static_cast <int> (v.size() / 2);

    std::nth_element (v.begin(), v.begin() + n, v.end());
    double vn = static_cast <double> (v [static_cast <size_t> (n)]);

    if (v.size() % 2 != 1)
    {
        std::nth_element (v.begin(), v.begin() + n - 1, v.end());
        vn = 0.5 * (vn + v [static_cast <size_t> (n) - 1]);
    }

    return vn;
}

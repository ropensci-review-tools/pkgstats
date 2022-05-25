#pragma once

#include <iostream>
#include <fstream>
#include <stdexcept>
#include <string>
#include <stdio.h>
#include <vector>
#include <numeric> // std::accumulate
#include <assert.h>
#include <cctype> // isspace

#include <cpp11.hpp>

#include "utils.h"

/* This is a self-contained and highly restricted version of a Lines-Of-Code
 * counting algorithm. It is nowhere near as comprehensive as libraries like 
 * https://github.com/boyter/scc or https://github.com/XAMPPRocky/tokei. The
 * intention is just to provide a self-contained version that counts the basic
 * statistics necessary for statistical analyses of R packages. An additional
 * intention is to quantify average white space within code, which conventional
 * libraries such as those linked above do not do.
 *
 * The routine returns the following 'class` objects with vectors holding values
 * for each line in one file of:
 *  - Numbers of leading white spaces
 *  - Total numbers of white spaces
 *  - Total numbers of non-white spaces
 * Plus one additional value of total numbers of empty (white) lines.
 */
class LocStats
{
    public:

        int nlines, ncode, ndoc, empty_lines, nbrackets;

        std::vector <int> leading, white, nonwhite, doc, tab;

        LocStats (const size_t n) {

            nlines = static_cast <int> (n);
            ncode = 0L;
            ndoc = 0L;
            empty_lines = 0L;
            nbrackets = 0L;

            leading.resize (n, 0L);
            white.resize (n, 0L);
            nonwhite.resize (n, 0L);
            doc.resize (n, 0L);

            tab.resize (n, 0L);
        }
};

namespace loc {

size_t file_nlines (std::ifstream &in_file);

LocStats file_loc (const std::string f,
        const std::string cmt_open,
        const std::string cmt_close,
        const std::string cmt);

} // end namespace loc

cpp11::writable::integers cpp_loc(const cpp11::strings flist,
        const cpp11::strings cmt_open,
        const cpp11::strings cmt_close,
        const cpp11::strings cmt);

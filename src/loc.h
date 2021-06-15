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

// Vectors holding values for each line in one file of:
// - Numbers of leading white spaces
// - Total numbers of white spaces
// - Total numbers of non-white spaces
// Plus one additional value of total numbers of empty (white) lines.
class LocStats
{
    public:

        int nlines, empty_lines;

        std::vector <int> leading, white, nonwhite, doc;

        LocStats (const size_t n) {

            nlines = static_cast <int> (n);
            empty_lines = 0L;

            leading.resize (n, 0L);
            white.resize (n, 0L);
            nonwhite.resize (n, 0L);
            doc.resize (n, 0L);
        }
};

namespace loc {

size_t file_nlines (std::ifstream &in_file);

LocStats file_loc (std::string f);

} // end namespace loc

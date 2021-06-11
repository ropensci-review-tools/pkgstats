#include <iostream>
#include <fstream>
#include <string>
#include <stdio.h>
#include <vector>
#include <numeric> // std::accumulate
#include <assert.h>
#include <cctype> // isspace

#include <cpp11.hpp>
using namespace cpp11;

// Vectors holding values for each line in one file of:
// - Numbers of leading white spaces
// - Total numbers of white spaces
// - Total numbers of non-white spaces
// Plus one additional value of total numbers of empty (white) lines.
class Spaces
{
    public:

        int nlines, empty_lines;

        std::vector <int> leading, white, nonwhite;

        Spaces (const size_t n) {

            nlines = static_cast <int> (n);
            empty_lines = 0L;

            leading.resize (n, 0L);
            white.resize (n, 0L);
            nonwhite.resize (n, 0L);
        }
};

size_t file_nlines (std::ifstream &in_file)
{
    in_file.clear ();
    in_file.seekg (0L);

    std::string line;

    size_t n = 0;
    while (std::getline (in_file, line, '\n'))
            n++;

    in_file.clear ();
    in_file.seekg (0L);

    return n;
}

Spaces file_white_space (std::string f)
{
    std::ifstream in_file;
    in_file.open (f.c_str (), std::ifstream::in);
    assert (!in_file.fail ());

    const size_t n = file_nlines (in_file);

    std::string line;

    Spaces spaces (n);
    size_t i = 0;

    while (std::getline (in_file, line, '\n'))
    {
        if (line.length () == 0L)
            spaces.empty_lines++;

        bool white = true;

        for (size_t j = 0; j < line.length (); j++) {
            white = white && isspace (line [j]);
            if (white)
                spaces.leading [i] = static_cast <int> (j);
            bool white_i = isspace (line [j]);
            spaces.white [i] += white_i;
            spaces.nonwhite [i] += !white_i;
        }
        i++;
    }

    return spaces;
}

// Returns a single vector with:
// - first element = total number of lines in all files
// - second element = total number of blank / empty lines
// - third element = total number of spaces
// - fourth element = total number of non-space characters
// - after that, a frequency table of first 50 counts of numbers of leading
// white spaces on each line
[[cpp11::register]]
writable::integers cpp_white_space(strings flist)
{
    const int nleading = 50;
    writable::integers res (nleading + 4L);
    std::fill (res.begin (), res.end (), 0L);

    const int n = static_cast <int> (flist.size ());

    for (int f = 0; f < n; f++)
    {
        Spaces spaces = file_white_space (flist [f]);

        res [0] += spaces.nlines;
        res [1] += spaces.empty_lines;

        res [2] += static_cast <int> (
                std::accumulate (spaces.white.begin (),
                    spaces.white.end (), 0L));
        res [3] += static_cast <int> (
                std::accumulate (spaces.nonwhite.begin (),
                    spaces.nonwhite.end (), 0L));
        for (auto i: spaces.leading)
            if (i < nleading)
                res [4 + i]++;
    }

    return res;
}

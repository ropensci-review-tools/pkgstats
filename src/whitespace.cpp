#include "code-symbols.h"
#include "whitespace.h"

#include <cpp11.hpp>
using namespace cpp11;

size_t whitespace::file_nlines (std::ifstream &in_file)
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

Spaces whitespace::file_white_space (std::string f)
{
    std::ifstream in_file;
    in_file.open (f.c_str (), std::ifstream::in);
    assert (!in_file.fail ());

    /*
     //
    //*
    //*
     */

    const size_t n = file_nlines (in_file);

    std::string line;

    const std::string cmt_open = "/*", cmt_close = "*/";

    const std::string junkstr = "/* plus some \
                                 more*/";

    Spaces spaces (n);
    size_t i = 0;
    bool in_quote = false, in_block_cmt = false;

    while (std::getline (in_file, line, '\n'))
    {
        if (line.length () == 0L)
            spaces.empty_lines++;

        std::vector <size_t> opens = codesymbols::get_sympos (line, cmt_open);
        std::vector <size_t> closes = codesymbols::get_sympos (line, cmt_close);
        std::vector <size_t> qpos = codesymbols::get_quote_pos (line);

        std::vector <size_t> temp = codesymbols::rm_syms_in_quotes (opens, qpos, in_quote);
        in_quote = temp.size () == 1;
        qpos = codesymbols::rm_syms_in_quotes (closes, qpos, in_quote);
        in_quote = qpos.size () == 1;

        codesymbols::balance_block_cmts (opens, closes);

        bool white = true;

        for (size_t j = 0; j < line.length (); j++) {

            bool white_i = isspace (line [j]);
            white = white && white_i;
            if (white)
                spaces.leading [i] = static_cast <int> (j);
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
        Spaces spaces = whitespace::file_white_space (flist [f]);

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

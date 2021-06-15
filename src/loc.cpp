#include "code-symbols.h"
#include "loc.h"

#include <cpp11.hpp>
using namespace cpp11;

size_t loc::file_nlines (std::ifstream &in_file)
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

LocStats loc::file_loc (const std::string f,
        const std::string cmt_open,
        const std::string cmt_close,
        const std::string cmt)
{
    std::ifstream in_file;
    in_file.open (f.c_str (), std::ifstream::in);
    assert (!in_file.fail ());

    const size_t n = file_nlines (in_file);

    std::string line;

    LocStats stats (n);
    size_t i = 0;
    bool quote_start = false,
         quote_end = false,
         in_quote = false,
         block_cmt_start = false,
         block_cmt_end = false,
         in_block_cmt = false;
    // Three bool vars aren't necessary, but make the code easier to understand.

    const bool has_block_cmts = cmt_open.size () > 0 && cmt_close.size () > 0;

    while (std::getline (in_file, line, '\n'))
    {
        const bool is_blank = codesymbols::line_is_blank (line);

        if (has_block_cmts)
        {
            std::vector <size_t> opens = codesymbols::get_sympos (line, cmt_open);
            std::vector <size_t> closes = codesymbols::get_sympos (line, cmt_close);
            std::vector <size_t> qpos = codesymbols::get_quote_pos (line);

            std::vector <size_t> temp = codesymbols::rm_syms_in_quotes (opens, qpos, in_quote);
            quote_start = temp.size () == 1;
            qpos = codesymbols::rm_syms_in_quotes (closes, qpos, in_quote);
            quote_end = qpos.size () == 1;

            in_quote = quote_start && !quote_end;
            if (in_quote && quote_end)
                in_quote = false;

            codesymbols::balance_block_cmts (opens, closes);
            block_cmt_start = (opens.size () == 1 && closes.size () == 0);

            if (!in_block_cmt && block_cmt_start)
                in_block_cmt = true;
            block_cmt_end = closes.size () != 0;
            if (block_cmt_end) {
                block_cmt_start = false;
                in_block_cmt = false;
            }
        }

        bool single_cmt = false;
        if (!in_quote && !in_block_cmt)
            single_cmt = codesymbols::is_comment (line, cmt);

        if (is_blank)
            stats.empty_lines++;
        else if (in_block_cmt || single_cmt)
            stats.ndoc++;
        else
            stats.ncode++;

        bool white = true;

        for (size_t j = 0; j < line.length (); j++) {

            bool white_i = isspace (line [j]);
            white = white && white_i;
            if (white)
                stats.leading [i] = static_cast <int> (j);
            stats.white [i] += white_i;
            stats.nonwhite [i] += !white_i;
        }
        i++;
    }

    return stats;
}

// Returns a single vector with:
// - [0] = total number of lines in all files
// - [1] = number of lines of code
// - [2] = number of lines of documentation (comments)
// - [3] = total number of blank / empty lines
// - [4] = total number of spaces
// - [5] = total number of non-space characters
// - after that, a frequency table of first 50 counts of numbers of leading
// white spaces on each line
[[cpp11::register]]
writable::integers cpp_loc(const strings flist,
        const strings cmt_open,
        const strings cmt_close,
        const strings cmt)
{
    const int nleading = 50;
    writable::integers res (nleading + 6L);
    std::fill (res.begin (), res.end (), 0L);

    const int n = static_cast <int> (flist.size ());

    for (int f = 0; f < n; f++)
    {
        LocStats stats = loc::file_loc (flist [f],
                cmt_open [f],
                cmt_close [f],
                cmt [f]);

        res [0] += stats.nlines;
        res [1] += stats.ncode;
        res [2] += stats.ndoc;
        res [3] += stats.empty_lines;

        res [4] += static_cast <int> (
                std::accumulate (stats.white.begin (),
                    stats.white.end (), 0L));
        res [5] += static_cast <int> (
                std::accumulate (stats.nonwhite.begin (),
                    stats.nonwhite.end (), 0L));
        for (auto i: stats.leading)
            if (i < nleading)
                res [6 + i]++;
    }

    return res;
}

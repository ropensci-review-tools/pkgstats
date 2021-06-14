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

std::vector <size_t> whitespace::get_sympos (const std::string &s, const std::string &sym)
{
    std::vector <size_t> out;

    size_t i = s.find (sym, 0L);
    while (i != std::string::npos)
    {
        out.push_back (i);
        i = s.find (sym, i + 1);
    }

    return out;
}

std::vector <size_t> whitespace::rm_syms_in_quotes (std::vector <size_t> &sympos,
        std::vector <size_t> qpos,
        const bool in_quote)
{
    if (in_quote)
    {
        if (qpos.size () == 0 || sympos.size () == 0)
            return qpos;

        while (sympos [0] < qpos [0])
        {
            sympos.erase (sympos.begin ());
            if (sympos.size () == 0)
                break;
        }

        qpos.erase (qpos.begin ());
    } // then no longer in quote

    if (qpos.size () == 1)
    {
        sympos.clear ();
    } else
    {
        while (qpos.size () > 2)
        {
            if (sympos.size () > 0)
            {
                while (sympos [0] > qpos [0] &&
                        sympos [0] < qpos [1])
                {
                    sympos.erase (sympos.begin ());
                    if (sympos.size () < 1)
                        break;
                }
            }
            qpos.erase (qpos.begin ());
            qpos.erase (qpos.begin ());
        }
    }

    return qpos;
}

/* Find positions of all quotes in input string excluding escaped quotations.
 */
std::vector <size_t> whitespace::get_quote_pos (const std::string &s)
{
    std::vector <size_t> qpos = whitespace::get_sympos (s, "\"");
    std::vector <size_t> qpos_esc = whitespace::get_sympos (s, "\\\"");

    // Align positions of qpos_esc with original qpos positions:
    for (auto q = qpos_esc.begin (); q < qpos_esc.end (); ++q)
        ++(*q);

    // Then remove any qpos that are also qpos_esc:
    for (auto q1 = qpos.begin (); q1 != qpos.end (); q1++)
    {
        bool is_esc = false;
        for (auto q2: qpos_esc)
            if (*q1 == q2)
                is_esc = true;
        if (is_esc)
        {
            qpos.erase (q1--);
        }
    }

    return qpos;
}

Spaces whitespace::file_white_space (std::string f)
{
    std::ifstream in_file;
    in_file.open (f.c_str (), std::ifstream::in);
    assert (!in_file.fail ());

    const size_t n = file_nlines (in_file);

    std::string line;

    const std::string cmt_open = "/*", cmt_close = "*/";

    Spaces spaces (n);
    size_t i = 0;
    bool in_quote = false, in_block_cmt = false;

    while (std::getline (in_file, line, '\n'))
    {
        if (line.length () == 0L)
            spaces.empty_lines++;

        std::vector <size_t> opens = whitespace::get_sympos (line, cmt_open);
        std::vector <size_t> closes = whitespace::get_sympos (line, cmt_close);
        std::vector <size_t> qpos = whitespace::get_quote_pos (line);

        std::vector <size_t> temp = whitespace::rm_syms_in_quotes (opens, qpos, in_quote);
        in_quote = temp.size () == 1;
        qpos = whitespace::rm_syms_in_quotes (closes, qpos, in_quote);
        in_quote = qpos.size () == 1;

        if (closes.size () > 0)
        {
            while (closes.size () >= opens.size ())
            {
                closes.erase (closes.begin ());
                if (opens.size () > 0)
                    opens.erase (opens.begin ());
                if (closes.size () == 0)
                    break;
            }
        }

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

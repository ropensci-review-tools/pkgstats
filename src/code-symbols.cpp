#include "code-symbols.h"

// Check whether a line contains only space characters
bool codesymbols::line_is_blank (std::string &s)
{
    if (s.size () == 0L)
        return true;

    bool blank = true;
    //for (auto i = s.begin (); i != std::prev (s.end ()); i++)
    for (auto i = s.begin (); i != s.end (); i++)
        blank = blank && isspace (*i);

    return blank;
}

// sym is simple comment symbol with no leading whitespace regex
bool codesymbols::is_comment (std::string s, const std::string sym)
{
    if (s.size () == 0L)
        return false;

    while (s.size () > 0 && isspace (s [0]))
        s = s.substr (1, s.size ());

    return s.find (sym, 0L) == 0L;
}

std::vector <size_t> codesymbols::get_sympos (const std::string &s, const std::string &sym)
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

std::vector <size_t> codesymbols::rm_syms_in_quotes (std::vector <size_t> &sympos,
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
std::vector <size_t> codesymbols::get_quote_pos (const std::string &s)
{
    std::vector <size_t> qpos = codesymbols::get_sympos (s, "\"");
    std::vector <size_t> qpos_esc = codesymbols::get_sympos (s, "\\\"");

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

void codesymbols::balance_block_cmts (std::vector <size_t> &opens,
        std::vector <size_t> &closes)
{

    if (closes.size () > 0 && opens.size () > 0)
    {
        while (closes.size () > opens.size () && opens.size () > 0)
        {
            closes.erase (closes.begin ());
            if (opens.size () > 0)
                opens.erase (opens.begin ());
            if (closes.size () == 0)
                break;
        }
    }
}

int codesymbols::count_brackets (std::string s, bool open)
{
    int n = 0L;

    std::vector <std::string> brackets;
    if (open)
        brackets = {"(", "[", "{"};
    else
        brackets = {")", "]", "}"};

    for (auto b: brackets)
    {
        auto it = s.find (b);
        while (it != std::string::npos) {
            n++;
            it = s.find (b, it + 1);
        }
    }

    return n;
}

#include <iostream>
#include <fstream>
#include <string>
#include <stdio.h>
#include <assert.h>

#include <cpp11.hpp>
using namespace cpp11;

#define BUFFER_SIZE 512

int rd_examples (std::string f)
{
    std::ifstream in_file;
    in_file.open (f.c_str (), std::ifstream::in);
    assert (!in_file.fail ());

    std::string line;
    std::getline (in_file, line, '\n');

    return 0;
}

[[cpp11::register]]
writable::integers cpp_parse_rd(strings flist)
{
    const int n = static_cast <int> (flist.size ());

    writable::integers res (n);
    std::fill (res.begin (), res.end (), 0L);

    for (int f = 0; f < n; f++)
    {
        res [f] = rd_examples (flist [f]);
    }


    return res;
}

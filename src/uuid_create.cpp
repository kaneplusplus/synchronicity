#include <iostream> // Hack to make sure we are using the correct length
                    // function
#include <cstring>  // Hack to make sure the correct memcpy is called
#include <cstdio>
#include <string>
#include <sstream>
#include <boost/uuid/uuid_generators.hpp>
#include <boost/uuid/uuid_io.hpp>

#include <R.h>
#include "synchronicity/util.h"

using namespace boost;

extern "C"
{

SEXP boost_create_uuid()
{
  std::stringstream ss;
  boost::uuids::basic_random_generator<boost::mt19937> gen;
  boost::uuids::uuid u = gen();
  ss << u;
  return String2RChar(ss.str());
}

} // extern "C"

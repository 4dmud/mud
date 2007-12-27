//
// C++ Interface: col_string
//
// Description: 
//
//
// Author: Jamie Nelson <mordecai@xtra.co.nz>, (C) 2006
//
// Copyright: See COPYING file that comes with this distribution
//
//


class cstring : public std::string
{
public:
  cstring();
  cstring(const std::string&);
  ~cstring();
  
//   const cstring& 
  void operator=(const char *);
  void operator+=(const char *);
  void operator+=(const std::string&);
  
  bool operator==(const std::string&);
  bool operator==(class cstring&);
  bool operator==(const char *);
  
  string c_str();
  const char *uc_str();
  
private:
  char *m_cstr;
  char *m_ucstr;
};

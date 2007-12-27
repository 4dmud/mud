#include <mysql/mysql.h>
#include <sys/time.h>

#include "conf.h"
#include "sysdep.h"
#include "structs.h"
#include "utils.h"
#include "db.h"

#include "interpreter.h"

MYSQL     *myDatabaseConnect_ptr;
MYSQL_RES *queryResult_ptr;
MYSQL_ROW  row;
unsigned int num_fields, num_rows, i;

ACMD(do_mysql)
{
        myDatabaseConnect_ptr = mysql_init(NULL);

	// Connect to the database
        if (mysql_real_connect(myDatabaseConnect_ptr, "YOUR.HOST", "YOUR.DB.USER", "YOUR.DB.PASS", "YOUR.DB", 0, "", 0) == NULL) {
		if (mysql_errno(myDatabaseConnect_ptr))
		  log("MYSQL: %s", mysql_error(myDatabaseConnect_ptr));
		return;
	}

	// Execute my query
	if (mysql_query(myDatabaseConnect_ptr, "YOUR.QUERY")) {
		if (mysql_errno(myDatabaseConnect_ptr))
                                log("MYSQL: %s", mysql_error(myDatabaseConnect_ptr));
		return;
	} else {
		queryResult_ptr = mysql_store_result(myDatabaseConnect_ptr);
		if (queryResult_ptr) {
			num_fields = mysql_num_fields(queryResult_ptr);

			// Fetch the result from the database			
			while((row = mysql_fetch_row(queryResult_ptr))) {
				log("%s", row[0]);
				log("%s", row[1]);
				log("%s", row[2]);
			}
			mysql_free_result(queryResult_ptr);
			mysql_close(myDatabaseConnect_ptr);
		} else { // mysql_store_result() returned nothing; should it have?
			if (mysql_errno(myDatabaseConnect_ptr)) {
				log("MYSQL: %s", mysql_error(myDatabaseConnect_ptr));
			} else if (mysql_field_count(myDatabaseConnect_ptr) == 0) {
				// query does not return data
            			// (it was not a SELECT)
            			num_rows = mysql_affected_rows(myDatabaseConnect_ptr);
        		}
		}
	}
	return;
}

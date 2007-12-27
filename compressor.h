//
// C++ Interface: compressor
//
// Description: 
//
//
// Author: Jamie Nelson <mordecai4d@gmail.com>, (C) 2007
//
// Copyright: See COPYING file that comes with this distribution
//
//

#define CHUNK 16384
typedef struct compress_map_data {
 unsigned char *saved;
 unsigned char *inflated;
 bool parsed;
 time_t last_accessed;
 size_t length;
 int copies;
 
 compress_map_data() {
	 saved = NULL;
	 inflated = NULL;
	 parsed = false;
	 last_accessed = 0;
	 length = 0;
	 copies = 1;
 }
 ~compress_map_data() {
	 copies -= 1;
	if (copies == 0 && inflated)
		 free(inflated);
	 inflated = NULL;
	 if (copies == 0 && saved)
		 free(saved);
	 saved = NULL;
 }
 
};

class Compressor {
	public:
		char *InflateFromId(long i);
		long CompressToId(const char *in, long i);
		long CompressToId(const char *in);
		long CompressToId(const char *in, long i, bool comp);
		long CompressToId(const char *in, bool comp);
		bool DeleteId(long i);
		int cleanInflated();
		const char * zerr(int ret);
		Compressor() {
			TopID = 0;
			ctr = NULL;
			/* allocate deflate state */
			istrm.zalloc = Z_NULL;
			istrm.zfree = Z_NULL;
			istrm.opaque = Z_NULL;
			istrm.avail_in = 0;
			istrm.next_in = Z_NULL;
			
			dstrm.zalloc = Z_NULL;
			dstrm.zfree = Z_NULL;
			dstrm.opaque = Z_NULL;//Z_DEFAULT_COMPRESSION
			deflateInit(&dstrm, 1);
			inflateInit(&istrm);
			bigs = 0;
			smalls = 0;
		}
		~Compressor() {
			/* clean up and return */
			(void)deflateEnd(&dstrm);
			(void)inflateEnd(&istrm);
			
			for (std::map<long, compress_map_data *>::iterator z = zmap.begin();z != zmap.end();z++)
				delete z->second;
			zmap.clear();
		}
		inline int Bigs() {
			return bigs;
		}
		inline int Smalls() {
			return smalls;
		}
	private:
		unsigned char * inf(unsigned char *source, size_t &dest);
		unsigned char * def(unsigned char *source, size_t &dest);
		z_stream dstrm;
		z_stream istrm;
		unsigned char *tmp;
		int dlen;
		string o_s;
		long TopID;
		std::map<long, compress_map_data *> zmap;
		unsigned char in[CHUNK];
		unsigned char out[CHUNK];
		unsigned char *ctr;
		int cur_size; 
		int last_size;
		int read_to;
		size_t r_len;
		int bigs;
		int smalls;
		
			
};

extern Compressor compressor;

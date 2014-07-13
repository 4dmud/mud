//
// C++ Implementation: compresser
//
// Description:
//
//
// Author: Jamie Nelson <mordecai4d@gmail.com>, (C) 2007
//
// Copyright: See COPYING file that comes with this distribution
//
//

#include <stdio.h>
#include <string.h>
#include <assert.h>
#include "config.h"
#include "sysdep.h"
#include "zlib.h"
#include "compressor.h"

class MudException;

#if defined(MSDOS) || defined(OS2) || defined(WIN32) || defined(__CYGWIN__)
#  include <fcntl.h>
#  include <io.h>
#  define SET_BINARY_MODE(file) setmode(fileno(file), O_BINARY)
#else
#  define SET_BINARY_MODE(file)
#endif

void basic_mud_log(const char *format, ...);
Compressor compressor;

/* Compress from file source to file dest until EOF on source.
   def() returns Z_OK on success, Z_MEM_ERROR if memory could not be
   allocated for processing, Z_STREAM_ERROR if an invalid compression
   level is supplied, Z_VERSION_ERROR if the version of zlib.h and the
   version of the library linked do not match, or Z_ERRNO if there is
   an error reading or writing the files. */
unsigned char * Compressor::def(unsigned char *source, size_t &dest) {
    int  flush;
    int ret = Z_OK;
    // int have;
    read_to = 0;
    r_len = 0;
    cur_size = 0;
    last_size = 0;
    //    unsigned char *buf_tmp = NULL;

    ctr = source;
    *out = '\0';

    bigs += (r_len = strlen((char *)ctr));
    //basic_mud_log("String passed (%d): %s", r_len,source);
    deflateReset(&dstrm);
    //buf_tmp = new unsigned char[1024];
    o_s.clear();
    /* compress until end of file */
    do {
        if (r_len < CHUNK)
            read_to = (r_len);
        else
            read_to = CHUNK;

        dstrm.avail_in = read_to;
        strlcpy((char*)in, (char *)ctr, read_to);
        ctr += read_to;
        r_len -= read_to;

        flush = (read_to < CHUNK) ? Z_FINISH : Z_NO_FLUSH;
        dstrm.next_in = in;
        //dlen = deflateBound(&dstrm, read_to);//((cur_size * sizeof(unsigned char)))
        //buf_tmp = new unsigned char[dlen];
        //dest = (unsigned char*)malloc(dlen);

        /* run deflate() on input until output buffer not full, finish
        compression if all of source has been read in */
        do {
            dstrm.avail_out = CHUNK;
            dstrm.next_out = out;
            ret = deflate(&dstrm, flush);   /* no bad return value */
	    //basic_mud_log(zerr(ret));
            assert(ret != Z_STREAM_ERROR);  /* state not clobbered */
            //have = CHUNK - dstrm.avail_out;
            last_size = cur_size;
            cur_size += dstrm.total_out;
	    //basic_mud_log("Avail In: %d In size: %d Out Size: %d Ret: %s",dstrm.avail_in, read_to, cur_size * sizeof(unsigned char), zerr(ret));
            /* if (buf_tmp == NULL)
              
             else
              buf_tmp = (unsigned char*)realloc(buf_tmp, (cur_size * sizeof(unsigned char)));
             if (buf_tmp == NULL) {
              basic_mud_log("Memory Allocation error in Compression");
              break;
            }*/
            //strncpy((char *)buf_tmp + (last_size-1), (char *)out, dstrm.total_out+1);
            o_s.append((const char*)out);
            /*
            	if (fwrite(out, 1, have, dest) != have || ferror(dest)) {
            	(void)deflateEnd(&strm);
            	return Z_ERRNO;
            }*/
        } while (dstrm.avail_out == 0);
	if (dstrm.avail_in != 0) {
		//deflateReset(&dstrm);
		//basic_mud_log("Cur_size: %d os.size(): %d", cur_size, o_s.size());
		throw MudException("Can't parse the compression 1.");
    }
	// assert(dstrm.avail_in == 0);     /* all input will be used */

        /* done when last data in file processed */
    } while (flush != Z_FINISH);
   // basic_mud_log(zerr(ret));
    assert(ret == Z_STREAM_END);        /* stream will be complete */
   // basic_mud_log("Cur_size: %d os.size(): %d", cur_size, o_s.size());
    if (cur_size != o_s.size()) {
	   // deflateReset(&dstrm);
	    basic_mud_log(zerr(ret));
        throw MudException("Can't parse the compression 2.");
}
    //tmp = (unsigned char*)malloc(cur_size+1);
    //strncpy((char *)tmp, (char *)o_s.c_str(), cur_size+1);
    //tmp[cur_size+1] = 0;
    //deflateReset(&dstrm);
    smalls += (dest = o_s.size());//o_s.size();
    tmp = (unsigned char*)strdup(o_s.c_str());
    o_s.clear();
    return tmp;
}

/* Decompress from file source to file dest until stream ends or EOF.
   inf() returns Z_OK on success, Z_MEM_ERROR if memory could not be
   allocated for processing, Z_DATA_ERROR if the deflate data is
   invalid or incomplete, Z_VERSION_ERROR if the version of zlib.h and
   the version of the library linked do not match, or Z_ERRNO if there
   is an error reading or writing the files. */
unsigned char * Compressor::inf(unsigned char *source, size_t &dest) {
    int ret = Z_OK;
    read_to = 0;
    r_len = 0;
    //int have;
    cur_size = 0;
    last_size = 0;
    if (!source)
        return NULL;
    ctr = source;
    r_len = dest;
    o_s.clear();
    tmp = NULL;
    *out = '\0';

    /* decompress until deflate stream ends or end of file */
    do {

        if (r_len < CHUNK)
            read_to = (r_len);
        else
            read_to = CHUNK;
        memcpy((void *)in, (void *)ctr, read_to);
        istrm.avail_in = read_to;
        ctr += read_to;
        r_len -= read_to;

        if (istrm.avail_in == 0)
            break;
        istrm.next_in = in;

        /* run inflate() on input until output buffer not full */
        do {
            istrm.avail_out = CHUNK;
            istrm.next_out = out;
            ret = inflate(&istrm, Z_NO_FLUSH);
            assert(ret != Z_STREAM_ERROR);  /* state not clobbered */
            switch (ret) {
            case Z_NEED_DICT:
                ret = Z_DATA_ERROR;     /* and fall through */
            case Z_DATA_ERROR:
            case Z_MEM_ERROR:
                (void)inflateReset(&istrm);
                return NULL;
            }
            //have = CHUNK - istrm.avail_out;
            last_size = cur_size;
            cur_size += istrm.total_out;
            //tmp = (unsigned char*)realloc(tmp, cur_size * sizeof(unsigned char));
            //strncpy((char *)tmp + (last_size), (char *)out, istrm.total_out+1);
            out[cur_size+1] = '\0';
            o_s.append((char *)out);
            /*if (fwrite(out, 1, have, dest) != have || ferror(dest)) {
            	(void)inflateEnd(&strm);
            	return Z_ERRNO;
            }*/
        } while (istrm.avail_out == 0);

        /* done when inflate() says it's done */
    } while (ret != Z_STREAM_END);

    /* clean up and return */
    (void)inflateReset(&istrm);
    //tmp[cur_size+1] = '\0';
    tmp = (unsigned char*)strdup(o_s.c_str());
    o_s.clear();
    return tmp;
}

/* report a zlib or i/o error */
const char * Compressor::zerr(int ret) {
    switch (ret) {
    case Z_ERRNO:
        return "zpipe: error writing stdout";
        break;
    case Z_STREAM_ERROR:
        return "zpipe: invalid compression level";
        break;
    case Z_DATA_ERROR:
        return "zpipe: invalid or incomplete deflate data";
        break;
    case Z_MEM_ERROR:
        return "zpipe: out of memory";
        break;
    case Z_VERSION_ERROR:
        return "zpipe: zlib version mismatch!";
    }
    return "zpipe: no error.";
}
long Compressor::CompressToId(const char *in) {

    long newId;
    if (!in)
        return -1;
    compress_map_data *cmps = new compress_map_data();
    if (strlen(in) < 80) {
        cmps->parsed = false;
        cmps->saved = (unsigned char *)strdup(in);
    } else {
        try {
            cmps->parsed = true;
            cmps->saved = def((unsigned char*)in, cmps->length);
        } catch (MudException &e) {
            cmps->parsed = false;
            cmps->saved = (unsigned char*)strdup(in);
        }
    }

    newId = TopID++;
    zmap[newId] = cmps;
    return newId;
}
long Compressor::CompressToId(const char *in, bool comp) {

    long newId;
    if (!in)
        return -1;
    compress_map_data *cmps = new compress_map_data();
    if (strlen(in) < 80 || !comp) {
        cmps->parsed = false;
        cmps->saved = (unsigned char *)strdup(in);
    } else {
        try {
            cmps->parsed = true;
            cmps->saved = def((unsigned char*)in, cmps->length);
        } catch (MudException &e) {
            cmps->parsed = false;
            cmps->saved = (unsigned char*)strdup(in);
        }
    }

    newId = TopID++;
    zmap[newId] = cmps;
    return newId;
}
long Compressor::CompressToId(const char *_in, long i) {
    map<long, compress_map_data *>::iterator it;
    if (!_in)
        return -1;
    compress_map_data *cmps = new compress_map_data();
    if (strlen(_in) < 80) {
        cmps->parsed = false;
        cmps->saved = (unsigned char*)strdup(_in);
    } else {
        try {
            cmps->parsed = true;
            cmps->saved = def((unsigned char*)_in, cmps->length);
        } catch (MudException &e) {
            cmps->parsed = false;
            cmps->saved = (unsigned char*)strdup(_in);
        }
    }

    it = zmap.find(i);
    if (it != zmap.end())
        delete zmap[i];
    if (i >= TopID)
        TopID = i+1;
    zmap[i] = cmps;

    return i;
}

long Compressor::CompressToId(const char *_in, long i, bool comp) {
    map<long, compress_map_data *>::iterator it;
    if (!_in)
        return -1;
    compress_map_data *cmps = new compress_map_data();
    if (strlen(_in) < 80 || !comp) {
        cmps->parsed = false;
        cmps->saved = (unsigned char*)strdup(_in);
    } else {
        try {
            cmps->parsed = true;
            cmps->saved = def((unsigned char*)_in, cmps->length);
        } catch (MudException &e) {
            cmps->parsed = false;
            cmps->saved = (unsigned char*)strdup(_in);
	    
        }
    }

    it = zmap.find(i);
    if (it != zmap.end())
        delete zmap[i];
    if (i >= TopID)
        TopID = i+1;
    zmap[i] = cmps;

    return i;
}

char *Compressor::InflateFromId(long i) {
    map<long, compress_map_data *>::iterator it;
    it = zmap.find(i);
    if (it == zmap.end())
        return NULL;
    else {
        compress_map_data *cmps = it->second;
        cmps->last_accessed = time(0);
        if (cmps->parsed == false)
            return (char*)cmps->saved;
        else if (cmps->inflated) {
            return (char*)cmps->inflated;
        } else {
            cmps->inflated = inf(cmps->saved, cmps->length);
            return (char*)cmps->inflated;
        }
    }
}

bool Compressor::DeleteId(long i) {
    map<long, compress_map_data *>::iterator it;
    it = zmap.find(i);
    if (it == zmap.end())
        return false;
    delete zmap[i];
    zmap.erase(i);
    return true;
}
int Compressor::cleanInflated() {
    map<long, compress_map_data *>::iterator it;
    time_t tnow = time(0);
    int count = 0;
    for (it = zmap.begin();it != zmap.end();it++)
        if (it->second->inflated && it->second->last_accessed < (tnow - (60*60))) {
            free(it->second->inflated);
            it->second->inflated = NULL;
            count++;
        }
    return count;
}

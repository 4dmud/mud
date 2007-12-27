/* ******************************************************************** *
 * FILE        : assemblies.h                  Copyright (C) 1999 Geoff Davis
*
 * USAGE: Definitions, constants and prototypes for assembly engine.   *
 * -------------------------------------------------------------------- *
 * 1999 MAY 07 gdavis/azrael@laker.net Initial implementation.         *
 * ******************************************************************** */

#if !defined( __ASSEMBLIES_H__ )
#define __ASSEMBLIES_H__

/* ******************************************************************** *
 * Preprocessor constants.                                             *
 * ******************************************************************** */

/* Assembly type: Used in ASSEMBLY.iAssemblyType */
#define ASSM_ASSEMBLE          101       // Assembly must be assembled.
#define ASSM_BAKE              102       // Assembly must be baked.
#define ASSM_BREW              103       // Assembly must be brewed.
#define ASSM_CRAFT             104       // Assembly must be crafted.
#define ASSM_FLETCH            105       // Assembly must be fletched.
#define ASSM_KNIT              106       // Assembly must be knitted.
#define ASSM_MAKE              107       // Assembly must be made.
#define ASSM_MIX               108       // Assembly must be mixed.
#define ASSM_THATCH            109       // Assembly must be thatched.
#define ASSM_WEAVE             110       // Assembly must be woven.
#define ASSM_FORGE             111       // Assembly must be woven.

#define MAX_ASSM               11      // Number of assembly types.

/* ******************************************************************** *
 * Type aliases.                                                       *
 * ******************************************************************** */

typedef struct assembly_data   ASSEMBLY;
typedef struct component_data  COMPONENT;

/* ******************************************************************** *
 * Structure definitions.                                              *
 * ******************************************************************** */

/* Assembly structure definition. */
struct assembly_data {
  long         lVnum;                  /* Vnum of the object assembled. */
  long         lNumComponents;         /* Number of components. */
  unsigned char        uchAssemblyType;        /* Type of assembly (ASSM_xxx).
*/
  struct component_data *pComponents;          /* Array of component info. */
};

/* Assembly component structure definition. */
struct component_data {
  bool         bExtract;               /* Extract the object after use. */
  bool         bInRoom;                /* Component in room, not inven. */
  long         lVnum;                  /* Vnum of the component object. */
};

/* ******************************************************************** *
 * Prototypes for assemblies.c.
*
 * ******************************************************************** */

void           assemblyBootAssemblies( void );
void           assemblySaveAssemblies( void );
void           assemblyListToChar( struct char_data *pCharacter );

bool           assemblyAddComponent( long lVnum, long lComponentVnum,
                 bool bExtract, bool bInRoom );
bool           assemblyCheckComponents( long lVnum, struct char_data
                 *pCharacter );
bool           assemblyCreate( long lVnum, int iAssembledType );
bool           assemblyDestroy( long lVnum );
bool           assemblyHasComponent( long lVnum, long lComponentVnum );
bool           assemblyRemoveComponent( long lVnum, long lComponentVnum );

int            assemblyGetType( long lVnum );

long           assemblyCountComponents( long lVnum );
long           assemblyFindAssembly( const char *pszAssemblyName );
long           assemblyGetAssemblyIndex( long lVnum );
long           assemblyGetComponentIndex( ASSEMBLY *pAssembly,
                 long lComponentVnum );

ASSEMBLY*      assemblyGetAssemblyPtr( long lVnum );

/* ******************************************************************** */

#endif


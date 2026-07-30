#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "shapefil.h"
#include "../nkf/libnkf.h"
//==============================================================
//
//==============================================================
SHPHandle hSHP;
DBFHandle hDBF;


int shpopen( const char *file ){
  hSHP = SHPOpen( file, "rb" );

  if( hSHP == NULL ) return 1;
  return 0;
}                                                                                                     


int shpclose( ){
  if( hSHP == NULL ) return 1;
  SHPClose( hSHP );
  hSHP = NULL;
  return 0;
}


int shpgetinfo( int *nEntity, int *type, char *typeName,
                double *minBound, double *maxBound ){
  if( hSHP == NULL ) return 1;
  *nEntity = hSHP->nRecords;
  *type = hSHP->nShapeType;
  strcpy(typeName, SHPTypeName(hSHP->nShapeType));
  for( int i = 0; i < 4; i++ ){
    minBound[i] = hSHP->adBoundsMin[i];
  }
  for( int i = 0; i < 4; i++ ){
    maxBound[i] = hSHP->adBoundsMax[i];
  }
  return 0;
}


int shpgetentityinfo(
    int iEntity, 
    int *nVertices, int *nParts, int *bMeasureIsUsed, 
    double *dfXMin, double *dfXMax, 
    double *dfYMin, double *dfYMax, 
    double *dfZMin, double *dfZMax, 
    double *dfMMin, double *dfMMax
  ){

  SHPObject *psShape;
  psShape = SHPReadObject( hSHP, iEntity );

  if( psShape == NULL ) return 1;

  *nVertices      = psShape->nVertices;
  *nParts         = psShape->nParts;
  *bMeasureIsUsed = psShape->bMeasureIsUsed;
  *dfXMin = psShape->dfXMin;
  *dfXMax = psShape->dfXMax;
  *dfYMin = psShape->dfYMin;
  *dfYMax = psShape->dfYMax;
  *dfZMin = psShape->dfZMin;
  *dfZMax = psShape->dfZMax;
  *dfMMin = psShape->dfMMin;
  *dfMMax = psShape->dfMMax;

  SHPDestroyObject( psShape );

  return 0;
}


int shpgetpanpartstart( int iEntity, int *panPartStart ){
  SHPObject *psShape;
  psShape = SHPReadObject( hSHP, iEntity );

  if( psShape == NULL ) return 1;

  for( int iPart = 0; iPart < psShape->nParts; iPart++ ){
    panPartStart[iPart] = psShape->panPartStart[iPart];
  }

  SHPDestroyObject( psShape );
  return 0;
}


int shpgetdata( int iEntity, 
                double *x, double *y, double *z, double *m ){
  SHPObject *psShape;
  psShape = SHPReadObject( hSHP, iEntity );

  if( psShape == NULL ) return 1;

  if( psShape->bMeasureIsUsed ){
    for( int i = 0; i < psShape->nVertices; i++ ){
      x[i] = psShape->padfX[i];
      y[i] = psShape->padfY[i];
      z[i] = psShape->padfZ[i];
      m[i] = psShape->padfM[i];
    }
  } else {
    for( int i = 0; i < psShape->nVertices; i++ ){
      x[i] = psShape->padfX[i];
      y[i] = psShape->padfY[i];
      z[i] = psShape->padfZ[i];
    }
  }

  SHPDestroyObject( psShape );
  return 0;
}


int dbfopen( const char *file ){
  hDBF = DBFOpen( file, "rb" );

  if( hDBF == NULL ) return 1;
  return 0;
}


int dbfclose( ){
  if( hDBF == NULL ) return 1;
  DBFClose( hDBF );
  hDBF = NULL;
  return 0;
}


void dbfgetfieldcount( int *nFields ){
  *nFields = DBFGetFieldCount(hDBF);
}


void dbfgetrecordcount( int *nRecords ){
  *nRecords = DBFGetRecordCount(hDBF);
}


int dbfgetfieldinfo( 
      int iField, 
      char *chNativeType, char *typeName, 
      char *title, int *nWidth, int *nDecimals ){

  DBFFieldType eType;

  *chNativeType = DBFGetNativeFieldType( hDBF, iField );

  eType = DBFGetFieldInfo( hDBF, iField, title, nWidth, nDecimals );
  if( eType == FTString )
    typeName = "String";
  else if( eType == FTInteger )
    typeName = "Integer";
  else if( eType == FTDouble )
    typeName = "Double";
  else if( eType == FTInvalid )
    typeName = "Invalid";
  else
    return 1;

  title = (char *)nkf_guess_convert( title );
  return 0;
}


int dbfgetrecord( 
       int iRecord, int iField, const char *typeChar,
       int *recslen, char *recs, int *reci, double *recd ){
  const char * s;

  *recslen = 0;
  recs[0] = '\0';
  *reci = 0;
  *recd = 0.0;

  switch( *typeChar ){

    case 'C':
      s = nkf_guess_convert( (char *)DBFReadStringAttribute( hDBF, iRecord, iField ) );

      *recslen = strlen(s);
      int i;
      for( i = 0; i < *recslen; i++ ){
        recs[i] = s[i];
      }
      return 0;

    case 'N':
      //printf("Field %d (N) %d\n", iField, DBFReadIntegerAttribute( hDBF, iRecord, iField ) );
      *reci = DBFReadIntegerAttribute( hDBF, iRecord, iField );
      return 0;

    case 'D':
      //printf("Field %d (D) %f\n", iField, DBFReadDoubleAttribute( hDBF, iRecord, iField) );
      *recd = DBFReadIntegerAttribute( hDBF, iRecord, iField );
      return 0;

    default:
      return 2;
  }
}

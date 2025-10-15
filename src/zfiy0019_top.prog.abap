*&---------------------------------------------------------------------*
*&  Include           ZFIY0019_TOP
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&  types
*&---------------------------------------------------------------------*

TYPES: BEGIN OF TY_OUT_AUX,
        LINEA(255) TYPE C,
END OF TY_OUT_AUX.

TYPES: BEGIN OF TY_RECORD,
                  F01(11) TYPE C,   "CUIT;
                  F02(30) TYPE C,   "Razon Social/Denominacion
                  F03(12) TYPE C,   "Categoria;
                  F04(10) TYPE C,   "Situacion;
                  F05(22) TYPE C,   "CBU;
                  F06(10) TYPE C,   "Fecha Actualizacion CBU;
                  F07(10) TYPE C,   "Fecha de Publicacion de Inclusion;
                  F08(10) TYPE C,   "Fecha de Publicacion de Suspension;
                  F09(10) TYPE C,   "Fecha de Levantamiento de Suspension
                  F10(10) TYPE C,   "Fecha de Notificación de Exclusion;
                  F11(10) TYPE C,   "Fecha Actualización Registro;
                  F12(30) TYPE C,   "Observaciones ;
                  F13(10) TYPE C,   "Fecha Generación;
                END OF TY_RECORD.

TYPES: BEGIN OF TY_DATA,
  BUKRS     TYPE BUKRS,
  LIFNR     TYPE LIFNR,
  CATEGORIA TYPE CHAR20,
  SITUACION TYPE CHAR20,
  WITHT     TYPE WITHT,
  WT_WITHCD TYPE WT_WITHCD,
END OF  TY_DATA.


TYPES: BEGIN OF TY_LFB1,
       LIFNR TYPE LIFNR,
       BUKRS TYPE BUKRS,
 END OF  TY_LFB1.

TYPES: BEGIN OF TY_LFA1,
       STCD1 TYPE STCD1,
       LIFNR TYPE LIFNR,
       NAME1 TYPE NAME1_GP,
       KTOKK TYPE KTOKK,
       BRSCH TYPE BRSCH,
       BUKRS TYPE BUKRS,
       G1    TYPE CHAR1,
       G2    TYPE CHAR1,
       G3    TYPE CHAR1,
       G4    TYPE CHAR1,
       I1    TYPE CHAR1,
       I2    TYPE CHAR1,
       BP    TYPE CHAR1,
END OF  TY_LFA1.

TYPES: BEGIN OF TY_OUTPUT,
       STCD1     TYPE STCD1,
       LIFNR     TYPE LIFNR,
       NAME1     TYPE NAME1_GP,
       KTOKK     TYPE KTOKK,
       BRSCH     TYPE BRSCH,
       CATEGORIA TYPE CHAR20,
       SITUACION TYPE CHAR20,
       WITHT     TYPE LFBW-WITHT,
       WT_WITHCD TYPE LFBW-WT_WITHCD,
END OF  TY_OUTPUT.


TYPE-POOLS: SLIS.

TYPES: W_SLIS_T_FIELDCAT_ALV TYPE SLIS_FIELDCAT_ALV OCCURS 1.

DATA:
*&---------------------------------------------------------------------*
*&       Tablas
*&---------------------------------------------------------------------*
        T_RECORD            TYPE STANDARD TABLE OF TY_RECORD,
        T_DATA              TYPE STANDARD TABLE OF TY_DATA,
        T_DOWNLOAD          TYPE STANDARD TABLE OF TY_OUT_AUX,
        T_OUT_AUX           TYPE STANDARD TABLE OF TY_OUT_AUX,
        T_OUTPUT            TYPE STANDARD TABLE OF TY_OUTPUT,
        T_LFA1              TYPE STANDARD TABLE OF TY_LFA1,
        TL_LFA1             TYPE STANDARD TABLE OF TY_LFA1,
        T_LFBW              TYPE STANDARD TABLE OF LFBW,
        T_LFB1              TYPE STANDARD TABLE OF TY_LFB1,
        T_BDCDATA           TYPE BDCDATA    OCCURS 0 WITH HEADER LINE,
        T_MESSTAB           TYPE BDCMSGCOLL OCCURS 0 WITH HEADER LINE,
        T_MES_SAL           TYPE BDCMSGCOLL OCCURS 0 WITH HEADER LINE,
        LAYOUT              TYPE SLIS_LAYOUT_ALV,
        T_FIELDCAT          TYPE SLIS_T_FIELDCAT_ALV,
        T_EVENTS            TYPE SLIS_T_EVENT,
        GT_LIST_TOP_OF_PAGE TYPE SLIS_T_LISTHEADER,
        GT_LIST_END_OF_LIST TYPE SLIS_T_LISTHEADER,

*&---------------------------------------------------------------------*
*&       Estructuras
*&---------------------------------------------------------------------*
        ST_DOWNLOAD  TYPE TY_OUT_AUX,
        ST_DATA      TYPE TY_DATA,
        ST_OUT_AUX   TYPE TY_OUT_AUX,
        ST_OUTPUT    TYPE TY_OUTPUT,
        ST_RECORD    TYPE TY_RECORD,
        ST_LFA1      TYPE TY_LFA1,
        ST_LFB1      TYPE TY_LFB1,
        ST_LFBW      TYPE LFBW,

*&---------------------------------------------------------------------*
*&      Variables
*&---------------------------------------------------------------------*
         V_ARCHIVO TYPE CHAR5,
         V_REPID   TYPE SY-REPID,
         V_SMS     TYPE CHAR1,
         V_PATH    TYPE RLGRAP-FILENAME.

*&---------------------------------------------------------------------*
*&      CONSTANTS
*&---------------------------------------------------------------------*
CONSTANTS: C_X  TYPE CHAR2 VALUE 'X',
* IVA
           C_IW TYPE CHAR2 VALUE 'IW', "IVA
           C_I1 TYPE CHAR2 VALUE 'I1',
           C_I2 TYPE CHAR2 VALUE 'I2',
* Ganacias
           C_GB TYPE CHAR2 VALUE 'GB',
           C_G1 TYPE CHAR2 VALUE 'G1', "Ganancia
           C_G2 TYPE CHAR2 VALUE 'G2', "Ganancia
           C_G3 TYPE CHAR2 VALUE 'G3', "Ganancia
           C_G4 TYPE CHAR2 VALUE 'G4'. "Ganancia

* Grupo de Cuentas
CONSTANTS: C_YCER TYPE LFA1-KTOKK VALUE 'YCER',
           C_YCOR TYPE LFA1-KTOKK VALUE 'YCOR'.

RANGES: R_KTOKK FOR LFA1-KTOKK.

R_KTOKK-SIGN = 'I'.
R_KTOKK-OPTION = 'EQ'.
R_KTOKK-LOW = 'YCOR'.  " Corredor
APPEND R_KTOKK.
R_KTOKK-LOW = 'YCER'.  " proveedor de granos
APPEND R_KTOKK.

* Ramo
CONSTANTS: C_100 TYPE LFA1-BRSCH VALUE '0100',
           C_101 TYPE LFA1-BRSCH VALUE '0101',
           C_102 TYPE LFA1-BRSCH VALUE '0102',
           C_103 TYPE LFA1-BRSCH VALUE '0103'.

RANGES: R_BRSCH FOR LFA1-BRSCH.

R_BRSCH-SIGN = 'I'.
R_BRSCH-OPTION = 'EQ'.
R_BRSCH-LOW = '0100'.
APPEND R_BRSCH.
R_BRSCH-LOW = '0101'.
APPEND R_BRSCH.
R_BRSCH-LOW = '0102'.
APPEND R_BRSCH.
R_BRSCH-LOW = '0103'.
APPEND R_BRSCH.

* Situacion
CONSTANTS: C_EXC TYPE CHAR10 VALUE '"EXCLUIDO"',
           C_ACT TYPE CHAR10 VALUE '"ACTIVO"',
           C_SUS TYPE CHAR10 VALUE '"Suspensió'.

* Categoria
CONSTANTS: C_CONT TYPE CHAR12 VALUE '"Contratista',
           C_CORR TYPE CHAR12 VALUE '"Corredor',
           C_PROD TYPE CHAR12 VALUE '"Productor',
           C_ACOP TYPE CHAR12 VALUE '"Acopiador',
           C_OTRO TYPE CHAR12 VALUE '"Otro"',
           C_PROV TYPE CHAR12 VALUE '"Proveedor d'.

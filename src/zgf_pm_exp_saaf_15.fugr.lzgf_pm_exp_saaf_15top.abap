FUNCTION-POOL ZGF_PM_EXP_SAAF_15.           "MESSAGE-ID ..
*
TABLES: ZTPM_EXP_P_SAAF.
*
*
TYPES: BEGIN OF Y_LOCAS,
         MATNR TYPE MARA-MATNR,
         MAKTX TYPE MAKT-MAKTX,
         MAKTG TYPE MAKT-MAKTG,
       END OF Y_LOCAS.
*
DATA: "it_return    TYPE STANDARD TABLE OF bapiret2,
  IT_EXP_P_SAAF  TYPE TABLE OF ZTPM_EXP_P_SAAF WITH HEADER LINE,
  IT_LOCAS       TYPE TABLE OF Y_LOCAS WITH HEADER LINE,
  IT_B_COMP_SAAF TYPE TABLE OF ZTPM_B_COMP_SAAF WITH HEADER LINE.
*
CONSTANTS:
  LC_ZTPM_B_COMP_SAAF(16) VALUE 'ZTPM_B_COMP_SAAF',
  LC_COMB(12)             VALUE 'COMBUSTIVEIS',
  LC_IBAU(4)              VALUE 'IBAU',
  LC_HORIM(9)             VALUE 'HORIMETRO',
  LC_ODOME(8)             VALUE 'ODOMETRO',
  LC_HERST(5)             VALUE 'HERST',
  LC_HERLD(5)             VALUE 'HERLD',
  LC_TYPBZ(5)             VALUE 'TYPBZ',
  LC_KEY_NUM(7)           VALUE 'KEY_NUM',
  LC_LICENSE_NUM(11)      VALUE 'LICENSE_NUM',
  LC_FUEL_PRI(8)          VALUE 'FUEL_PRI',
  LC_FUEL_SEC(8)          VALUE 'FUEL_SEC',
  LC_CARD_NUM(8)          VALUE 'CARD_NUM',
  LC_FLEET_NUM(9)         VALUE 'FLEET_NUM',
*
  LC_TP_OBJ(6)            VALUE 'TP_OBJ',
  LC_FPN(8)               VALUE 'FPN-0010',
  LC_FMOT(8)              VALUE 'F-MOTIVO',
  LC_EQUI(4)              VALUE 'EQUI',
  LC_MAGI(4)              VALUE 'MAGI',
  LC_PT(2)                VALUE 'PT',
  LC_X(1)                 VALUE 'X',
  LC_S(1)                 VALUE 'S',
  LC_F(1)                 VALUE 'F',
  LC_C(1)                 VALUE 'C',
  LC_H(1)                 VALUE 'H',
  LC_R(1)                 VALUE 'R',
  LC_0(1)                 VALUE '0',
  LC_1(1)                 VALUE '1',
  LC_002(3)               VALUE '002',
  LC_I0076(5)             VALUE 'I0076',
  LC_I0320(5)             VALUE 'I0320',
  LC_ZPPM001(7)           VALUE 'ZPPM001'.

DATA:
  LV_TIMES(30)      TYPE C,
  LV_LANGU          TYPE BAPI_ITOB_PARMS-LANGU,
  LV_TPLNR          TYPE BAPI_ITOB_PARMS-FUNCLOC_INT,
  LV_KM_REAL_DEC    TYPE P DECIMALS 2,
  LV_KM_REAL_CH(30) TYPE C,
  LV_X              TYPE RMCLM-ANZUKZ,
  LV_002            TYPE KLAH-KLART,
  LV_ADRNR          TYPE ADRC-ADDRNUMBER,
  LV_OBJECT         TYPE AUSP-OBJEK,
  LV_TIMESS(30)     TYPE C,
  LV_DATA           TYPE UDATE,
  LV_TIME           TYPE UZEIT,
  LV_POINT          TYPE DIIMPT-POINT.

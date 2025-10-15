*&---------------------------------------------------------------------*
*& Report  Z_SPED_CONTABIL
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

report  Z_SPED_CONTABIL  line-size 245 message-id J_1BECD.


type-pools ABAP.
type-pools RSFS.
type-pools RSDS.
type-pools GUSL.

* data definition
include J_1BECD_MAIN_TOP.
include J_1BECD_MAIN_CON.

*" Exceção para o REG J150 versão 003 somente 6
*TYPES: BEGIN OF J_1BECD_J150_3_S_003,
*         REG           TYPE  J_1BECD_J150_3_S-REG,
*         COD_AGL       TYPE  J_1BECD_J150_3_S-COD_AGL,
*         NIVEL_AGL     TYPE  J_1BECD_J150_3_S-NIVEL_AGL,
*         DESCR_COD_AGL TYPE  J_1BECD_J150_3_S-DESCR_COD_AGL,
*         VL_CTA        TYPE  J_1BECD_J150_3_S-VL_CTA,
*         IND_VL        TYPE  J_1BECD_J150_3_S-IND_VL,
**       VL_CTA_ULT_DRE   TYPE  J_1BECD_J150_3_S-VL_CTA_ULT_DRE,
**       IND_VL_ULT_DRE   TYPE  J_1BECD_J150_3_S-IND_VL_ULT_DRE,
*       END OF J_1BECD_J150_3_S_003.
types: begin of J_1BECD_J930_3_S_006,                       "2463202
         REG            type J_1BECD_J930_3_S-REG,          "2463202
         IDENT_NOM      type J_1BECD_J930_3_S-IDENT_NOM,    "2463202
*---> 01/06/2023 - Migração S4 - JS
*         IDENT_CPF_CNPJ TYPE J_1BECD_J930_3_S-IDENT_CPF_CNPJ, "2463202
         IDENT_CPF_CNPJ type CHAR14,                        "2463202
*<--- 01/06/2023 - Migração S4 - JS
         IDENT_QUALIF   type J_1BECD_J930_3_S-IDENT_QUALIF, "2463202
         COD_ASSIN      type J_1BECD_J930_3_S-COD_ASSIN,    "2463202
         IND_CRC        type J_1BECD_J930_3_S-IND_CRC,      "2463202
         EMAIL          type J_1BECD_ACCOUNT_EMAIL,         "2463202
         FONE(11)       type C,                             "2463202
         UF_CRC         type J_1BECD_UF_CRC,                "2463202
         NUM_SEQ_CRC    type J_1BECD_NUM_SEQ_CRC,           "2463202
         DT_CRC         type J_1BECD_DT_CRC,                "2463202
*---> 01/06/2023 - Migração S4 - JS
*         IND_RESP_LEGAL TYPE J_1BECD_J930_3_S-IND_RESP_LEGAL,
         IND_RESP_LEGAL type CHAR1,
*<--- 01/06/2023 - Migração S4 - JS
       end of J_1BECD_J930_3_S_006.                         "2463202

*----------------------------------------------------------------------*
* ESTRUTURA SPED
*----------------------------------------------------------------------*
data: LS_RESULT     type TP_RESULT_LINE,
      LV_FIELD_MOE  type TP_RESULT_LINE,
      LV_ACCOUNT_NR type J_1BECD_ACCOUNT_CODE,
      W_TOTAL_0990  type I,
      W_TOTAL_I990  type I,
      W_TOTAL_J990  type I,
      W_TOTAL_9990  type I,
      W_TOTAL_J900  type I,
      W_TOTAL       type I,
      TABIX         type SY-TABIX.
*        GV_VERSN TYPE VERSN_011. "For selection screen validation

data:
  V2_SETNAME  type SETLEAF-SETNAME,
  V3_LINEID   type SETLEAF-LINEID,
  V4_SETCLASS type SETLEAF-SETCLASS,
  V5_DESCRIPT type SETLINET-DESCRIPT.


clear:  V2_SETNAME, V3_LINEID,  V4_SETCLASS, V5_DESCRIPT  .
types: begin of TY_BKPF,
         BUKRS type BKPF-BUKRS,
         BELNR type BKPF-BELNR,
         GJAHR type BKPF-GJAHR,
         BUDAT type BKPF-BUDAT,
         BLART type BKPF-BLART,
         XBLNR type BKPF-XBLNR,
       end of TY_BKPF,

       begin of TY_BSEG,
         BUKRS  type BSEG-BUKRS,
         BELNR  type BSEG-BELNR,
         GJAHR  type BSEG-GJAHR,
         BUZEI  type BSEG-BUZEI,
         SHKZG  type BSEG-SHKZG,
         DMBE2  type BSEG-DMBTR,
         HKONT  type BSEG-HKONT,
         DMBTR  type BSEG-DMBTR,
         SGTXT  type BSEG-SGTXT,
         XBILK  type BSEG-XBILK,
         VBUND  type BSEG-VBUND,
         BUDAT  type BKPF-BUDAT,
         DEL(1),
       end of TY_BSEG,

       begin of TY_DEL,
         HKONT type BSEG-HKONT,
         QTD   type BSEG-DMBTR,
       end of TY_DEL,

       begin of TY_DOC,
         BUKRS   type BSEG-BUKRS,
         BELNR   type BSEG-BELNR,
         GJAHR   type BSEG-GJAHR,
         BUDAT   type BKPF-BUDAT,
         TOTAL   type BSEG-DMBTR,
         TOTALF  type BSEG-DMBE2,
         XBLNR   type BKPF-XBLNR,
         TIPO(1),
       end of TY_DOC,

       begin of TY_AGL,
         ACCOUNT_NR      type J_1BECD_ACCOUNT_CODE,
         ALTKT           type J_1BECD_ACCOUNT_CODE,         "1408160
         SPED_INT_CODE   type J_1BECD_I050_3_S-COD_NAT,
         LEVEL           type J_1BECD_SINTETIC_ACC_S-FSV_LEVEL,
         SUPERIOR_ACC_NR type J_1BECD_ACCOUNT_CODE,
         DESCRIPTION     type J_1BECD_ACCOUNT_DESC,
         INCLUSION_DATE  type ERDAT_RF,
         IS_ANALY_SINT   type J_1BECD_ACCOUNT_TYPE,
         IS_EMPTY        type ABAP_BOOL, "Is empty node
         TOTAL_INI       type BSEG-DMBTR,
         TOTAL_FIN       type BSEG-DMBTR,
         TOTAL           type BSEG-DMBTR,
         TOTAL_INI_F     type BSEG-DMBE2,
         TOTAL_FIN_F     type BSEG-DMBE2,
         TOTALF          type BSEG-DMBE2,
         TOTAL_PER_A     type BSEG-DMBE2,
       end of TY_AGL,

       begin of TY_ENC,
         HKONT  type BSEG-HKONT,
         TOTAL  type BSEG-DMBTR,
         TOTALF type BSEG-DMBE2,
       end of TY_ENC,

       begin of TY_SLD,
         MES     type MONAT,
         HKONT   type BSEG-HKONT,
         VL_DEB  type BSEG-DMBTR,
         VL_CRE  type BSEG-DMBTR,
         VL_DEBF type BSEG-DMBTR,
         VL_CREF type BSEG-DMBTR,
       end of TY_SLD,

       begin of TY_CONTADOR,
         REG_BLC     type J_1BECD_REGISTER,
         QTD_REG_BLC type BSEG-DMBTR,
       end of TY_CONTADOR.

data: T_BKPF     type standard table of TY_BKPF,
      W_BKPF     type TY_BKPF,
      T_BSEG     type standard table of TY_BSEG,
      W_BSEG     type TY_BSEG,
      T_SLD      type table of TY_SLD,
      W_SLD      type TY_SLD,
      T_DOC      type table of TY_DOC,
      W_DOC      type TY_DOC,
      T_ENC      type table of TY_ENC,
      W_ENC      type TY_ENC,
      T_AGL      type table of TY_AGL,
      T_AGL_A    type table of TY_AGL,
      W_AGL      type TY_AGL,
      W_AGL_A    type TY_AGL,
      W_AGL2     type TY_AGL,
      T_DEL      type table of TY_DEL,
      W_DEL      type TY_DEL,
      T_CONTADOR type table of TY_CONTADOR,
      W_CONTADOR type TY_CONTADOR.

data: IT_CONTAS         type ZCT_EMP_CONTAS,
      WA_CONTAS         type ZLC_EMP_CONTAS,
      IT_SALDO_CONTAS   type table of ZDE_FI_GL_SALDO_FAGLFLEXT with header line,
      IT_SALDO_CONTAS_2 type table of ZDE_FI_GL_SALDO_FAGLFLEXT with header line,
      IT_SALDO_CONTAS_3 type table of ZDE_FI_GL_SALDO_FAGLFLEXT with header line.

data: REFE1        type HSLXX12,
      VMES         type MONAT,
      VMES_INI     type MONAT,
      VMES2        type MONAT,
      V_DMBTR      type BSEG-DMBTR,
      V_POS        type I,
      V_POS2       type I,
      V_SIN(1),
      V_DC(1),
      V_TEM(1),
      WL_SALDO_MI2 type FAGLFLEXT-KSLVT,
      WL_SALDO_MI3 type FAGLFLEXT-KSLVT.

*-----------------------------------------------------------------------
*- General data
*-----------------------------------------------------------------------
selection-screen begin of block GENERAL_DATA with frame title text-021.
  parameters:
    P_BUKRS like BKPF-BUKRS obligatory,
    P_GJAHR type GJAHR obligatory.
  select-options: S_MONAT for BKPF-MONAT no-extension obligatory.
  select-options: S_SAKNR for SKA1-SAKNR.
  select-options: S_RLDNR for BKPF-RLDNR modif id LDG
                                         no-extension
                                         no intervals
                                         matchcode object FAGL_RLDNR.
  parameter:                                                "1378742
    P_LEDGER like T881-RLDNR modif id SLD obligatory.       "1378742
  parameters: R_MOEDA    like BSID-UMSKZ as checkbox  default ' '.
selection-screen end of block GENERAL_DATA.

selection-screen: function key 4.

*-----------------------------------------------------------------------
*- File Creation Control Block
*-----------------------------------------------------------------------
selection-screen begin of block FILECONTROL with frame title text-025.
  selection-screen begin of tabbed block  TABB3 for 11 lines. "1378742
    selection-screen tab (23) text-043 user-command A8 default screen 0800.
    selection-screen tab (23) text-046 user-command B8 default screen 0808.
    selection-screen tab (23) text-036 user-command AC default screen 0850.
    selection-screen tab (20) text-017 user-command A7 default screen 0700.
  selection-screen end of block TABB3.
  parameters:
*-- Ignore BAdI implementation
    P_BADI type J_1BECD_BADI_OFF no-display.
selection-screen end of block FILECONTROL.

*-----------------------------------------------------------------------
*- Input Data Block
*-----------------------------------------------------------------------
selection-screen begin of block INPUT with frame title text-014.
  selection-screen begin of tabbed block  TABB2 for 5 lines.
    selection-screen tab (20) text-019 user-command CD default screen 0400.
    selection-screen tab (23) text-018 user-command A1 default screen 0050.
    selection-screen tab (23) text-008 user-command AS default screen 0200.
    selection-screen tab (20) text-033 user-command A9 default screen 0900.
    selection-screen tab (25) text-047 user-command A10 default screen 0901.
    selection-screen tab (20) text-035 user-command R1 default screen 0950 modif id SUB.
  selection-screen end of block TABB2.
selection-screen end of block INPUT.

*-----------------------------------------------------------------------
*- Selection screen for Company Data
*-----------------------------------------------------------------------
selection-screen begin of screen 0400 as subscreen.
  parameters: P_DTARQ  type J_1BECD_I030_3_S-DT_ARQ,
              P_DTCONV type J_1BECD_I030_3_S-DT_ARQ_CONV,
              P_NIRE   type J_1BECD_NIRE,
              P_INDNIR type J_1BECD_IND_NIRE default '0' value check, "1909036
              P_NIRSUB type J_1BECD_NIRE,                   "1909036
              P_NIRHAS type J_1BECD_HASH_CODE,              "1909036
              P_INDEGP type J_1BECD_IND_EMP_GRD_PRT,        "1961806
              P_TIPECD type J_1BECD_TIP_ECD,                "2154499
              P_CODSCP type J_1BECD_COD_SCP.                "2154499
  parameters  P_TRI as checkbox.                            "2190937
*---> 01/06/2023 - Migração S4 - JS
*PARAMETERS  P_IDNTMF   TYPE J_1BECD_IDENT_MF NO-DISPLAY DEFAULT 'N'. "2272317
  parameters  P_IDNTMF   type CHAR1 no-display default 'N'. "2272317
*<--- 01/06/2023 - Migração S4 - JS
selection-screen end of screen 0400.

*-----------------------------------------------------------------------
*- Selection screen for Special Situation
*-----------------------------------------------------------------------
selection-screen begin of screen 0808 as subscreen.
  selection-screen begin of block ESP.
    parameters: P_INDESP type J_1BECD_IND_SIT_ESP,
                P_DTESPI type J_1BECD_DT_INI_SIT,
                P_DTESPF type J_1BECD_DT_FIN_SIT,
                P_INIPER type J_1BECD_IND_INI_PER default '0' value check. "1909036
  selection-screen end of block ESP.
selection-screen end of screen 0808.

*-----------------------------------------------------------------------
*- Selection screen for Accounting Files (Journal + Subsidiary Journal)
*-----------------------------------------------------------------------
selection-screen begin of screen 0050 as subscreen.
  parameters: P_NUMORD type J_1BECD_NUM_ORD,
              P_NATLIV type J_1BECD_NAT_LIVR,
              P_PERCLS type J_1BECD_DT_FIN_SIT,             "1909036
              P_NUMO_S type J_1BECD_NUMO_S no-display,       "Only I012
              P_NATL_S type J_1BECD_NATL_S no-display,       "Only I012
              P_TIPO   type J_1BECD_BOOK_TYPE no-display,    "Only I012
              P_HASH_C type J_1BECD_HASH_CODE no-display,    "Only I012
              P_ACCRES type HKONT no-display.                "Only I015
selection-screen end of screen 0050.

*-----------------------------------------------------------------------
*- Selection screen for Accounting Statement
*-----------------------------------------------------------------------
selection-screen begin of screen 0200 as subscreen.
  parameters: P_IDDEM  type J_1BECD_ID_DEM,
              P_CABDEM type J_1BECD_CAB_DEM.
selection-screen end of screen 0200.

*-----------------------------------------------------------------------
*- Selection screen for Output file
*-----------------------------------------------------------------------
selection-screen begin of screen 0700 as subscreen.
  selection-screen begin of block OF.
*-- Presentation server
    parameter:  P_XPCOUT  type XFELD radiobutton group OUT.
    parameter:  P_PCOUT   type LOCALFILE.
*-- Application server
    parameter:  P_XAPOUT  type XFELD radiobutton group OUT.
    parameter:  P_APOUT   type LOCALFILE.
*-- Test output on screen
    parameter:  P_TEST    type J_1BECD_TEST_OUTPUT radiobutton group OUT
                default 'X'.
  selection-screen end of block OF.
selection-screen end of screen 0700.

*-----------------------------------------------------------------------
*- Selection screen for File Parameters
*-----------------------------------------------------------------------
selection-screen begin of screen 0800 as subscreen.
  parameters:
    P_SPRA   type J_1BECD_LANGU obligatory,                 "1325556
    P_INDESC type J_1BECD_IND_ESC obligatory.
  selection-screen begin of line.
    selection-screen comment 1(27) text-037 for field P_FSTAT.
    selection-screen position 35.
    parameters:
      P_FSTAT  type J_1BECD_VERSN obligatory.
  selection-screen end of line.
  parameters:
    P_FSIND  type J_1BECD_FSIND,
    P_ALTKT  type ALLGALTK,                                 "1408160
*  P_LAYOUT TYPE J_1BECD_COD_VER DEFAULT '004' VALUE CHECK.       "2154499 "2272317
    P_LAYOUT type J_1BECD_COD_VER default '004'.       "2154499 "2272317

  parameters:                                               "1909036
    P_ESC  type J_1BECD_IND_FIN_ESC default '0' value check, "1909036
*---> 01/06/2023 - Migração S4 - JS
*  P_CONS TYPE DE_J_1BECD_IND_ESC_CONS DEFAULT 'N' VALUE CHECK. "2416906
    P_CONS type ZDE_J_1BECD_IND_ESC_CONS default 'N' value check. "2416906
*<--- 01/06/2023 - Migração S4 - JS
  selection-screen begin of block CATEGORY with frame title text-039. "1378742
    parameters:                                             "1378742
      P_DEF  type J_1BECD_DEFAULT_CONTROL radiobutton group CAT default 'X', "1378742
      P_CUST type J_1BECD_CUST radiobutton group CAT,       "1378742
      P_VAR  type J_1BECD_TYPE_VARIANT radiobutton group CAT modif id SLD. "1378742
  selection-screen end of block CATEGORY.                   "1378742
selection-screen end of screen 0800.

*-----------------------------------------------------------------------
*- Selection screen for Closing file
*-----------------------------------------------------------------------
selection-screen begin of screen 0850 as subscreen.
  selection-screen begin of block CLS.
    parameters:
      P_DOCTYP type J_1BECD_CLOSDOC.
    parameters:
      P_DTRES  type J_1BECD_CLOSDATE.
    parameters P_YECP as checkbox.                          "2040307
  selection-screen end of block CLS.
selection-screen end of screen 0850.

*-----------------------------------------------------------------------
*- Selection screen for Attachments
*-----------------------------------------------------------------------
selection-screen begin of screen 0900 as subscreen.
*-- Presentation server
*---> 01/06/2023 - Migração S4 - JS
  "parameter:  P_TYPDOC  type J_1BECD_TIPO_DOC.
  "parameter:  P_DESRTF  type J_1BECD_DESC_RTF.
  parameter:  P_TYPDOC  type J_1BFGROUP.
  parameter:  P_DESRTF  type CHAR255.
*<--- 01/06/2023 - Migração S4 - JS

  parameter:  P_XPCIN  type XFELD radiobutton group IN default 'X'.
  parameter:  P_PCIN   type J_1BECD_ATTACHMENT.
*-- Application server
  parameter:  P_XAPIN  type XFELD radiobutton group IN.
  parameter:  P_APIN   type J_1BECD_ATTACHMENT.
selection-screen end of screen 0900.

*-----------------------------------------------------------------------
*- Selection screen for Attachments: Substitution
*-----------------------------------------------------------------------
selection-screen begin of screen 0901 as subscreen.
*---> 01/06/2023 - Migração S4 - JS
*  parameter:  P_STYPDO  type J_1BECD_TIPO_DOC_SUBSTITUTION default 001.
*  parameter:  P_SDESRT  type J_1BECD_DESC_RTF.
  parameter:  P_STYPDO  type J_1BFGROUP default 001.
  parameter:  P_SDESRT  type CHAR255.
*<--- 01/06/2023 - Migração S4 - JS
*-- Presentation server
  parameter:  P_SXPCIN  type XFELD radiobutton group INS default 'X'.
  parameter:  P_SPCIN   type J_1BECD_ATTACHMENT.
*-- Application server
  parameter:  P_SXAPIN  type XFELD radiobutton group INS.
  parameter:  P_SAPIN   type J_1BECD_ATTACHMENT.
selection-screen end of screen 0901.


*-----------------------------------------------------------------------
*- Selection screen for Subleger/Hash Code info for Bokkp. R and B
*-----------------------------------------------------------------------
selection-screen begin of screen 0950 as subscreen.

  selection-screen begin of line.
    selection-screen comment 1(15) C_SUBL01 modif id SL1.
    selection-screen comment 16(12) text-044 for field P_NUMO01 modif id SL1.
    parameter: P_NUMO01 type J_1BECD_NUMO_S modif id SL1.
    selection-screen comment 40(10) text-045 for field P_HASH01 modif id SL1.
    parameter: P_HASH01 type J_1BECD_HASH_CODE modif id SL1.
  selection-screen end of line.

  selection-screen begin of line.
    selection-screen comment 1(15) C_SUBL02 modif id SL2.
    selection-screen comment 16(12) text-044 for field P_NUMO02 modif id SL2.
    parameter: P_NUMO02 type J_1BECD_NUMO_S modif id SL2.
    selection-screen comment 40(10) text-045 for field P_HASH02 modif id SL2.
    parameter: P_HASH02 type J_1BECD_HASH_CODE modif id SL2.
  selection-screen end of line.

  selection-screen begin of line.
    selection-screen comment 1(15) C_SUBL03 modif id SL3.
    selection-screen comment 16(12) text-044 for field P_NUMO03 modif id SL3.
    parameter: P_NUMO03 type J_1BECD_NUMO_S modif id SL3.
    selection-screen comment 40(10) text-045 for field P_HASH03 modif id SL3.
    parameter: P_HASH03 type J_1BECD_HASH_CODE modif id SL3.
  selection-screen end of line.

  selection-screen begin of line.
    selection-screen comment 1(15) C_SUBL04 modif id SL4.
    selection-screen comment 16(12) text-044 for field P_NUMO04 modif id SL4.
    parameter: P_NUMO04 type J_1BECD_NUMO_S modif id SL4.
    selection-screen comment 40(10) text-045 for field P_HASH04 modif id SL4.
    parameter: P_HASH04 type J_1BECD_HASH_CODE modif id SL4.
  selection-screen end of line.

  selection-screen begin of line.
    selection-screen comment 1(15) C_SUBL05 modif id SL5.
    selection-screen comment 16(12) text-044 for field P_NUMO05 modif id SL5.
    parameter: P_NUMO05 type J_1BECD_NUMO_S modif id SL5.
    selection-screen comment 40(10) text-045 for field P_HASH05 modif id SL5.
    parameter: P_HASH05 type J_1BECD_HASH_CODE modif id SL5.
  selection-screen end of line.

  selection-screen begin of line.
    selection-screen comment 1(15) C_SUBL06 modif id SL6.
    selection-screen comment 16(12) text-044 for field P_NUMO06 modif id SL6.
    parameter: P_NUMO06 type J_1BECD_NUMO_S modif id SL6.
    selection-screen comment 40(10) text-045 for field P_HASH06 modif id SL6.
    parameter: P_HASH06 type J_1BECD_HASH_CODE modif id SL6.
  selection-screen end of line.

  selection-screen begin of line.
    selection-screen comment 1(15) C_SUBL07 modif id SL7.
    selection-screen comment 16(12) text-044 for field P_NUMO07 modif id SL7.
    parameter: P_NUMO07 type J_1BECD_NUMO_S modif id SL7.
    selection-screen comment 40(10) text-045 for field P_HASH07 modif id SL7.
    parameter: P_HASH07 type J_1BECD_HASH_CODE modif id SL7.
  selection-screen end of line.

  selection-screen begin of line.
    selection-screen comment 1(15) C_SUBL08 modif id SL8.
    selection-screen comment 16(12) text-044 for field P_NUMO08 modif id SL8.
    parameter: P_NUMO08 type J_1BECD_NUMO_S modif id SL8.
    selection-screen comment 40(10) text-045 for field P_HASH08 modif id SL8.
    parameter: P_HASH08 type J_1BECD_HASH_CODE modif id SL8.
  selection-screen end of line.

  selection-screen begin of line.
    selection-screen comment 1(15) C_SUBL09 modif id SL9.
    selection-screen comment 16(12) text-044 for field P_NUMO09 modif id SL9.
    parameter: P_NUMO09 type J_1BECD_NUMO_S modif id SL9.
    selection-screen comment 40(10) text-045 for field P_HASH04 modif id SL9.
    parameter: P_HASH09 type J_1BECD_HASH_CODE modif id SL9.
  selection-screen end of line.

  selection-screen begin of line.
    selection-screen comment 1(15) C_SUBL10 modif id SL0.
    selection-screen comment 16(12) text-044 for field P_NUMO10 modif id SL0.
    parameter: P_NUMO10 type J_1BECD_NUMO_S modif id SL0.
    selection-screen comment 40(10) text-045 for field P_HASH04 modif id SL0.
    parameter: P_HASH10 type J_1BECD_HASH_CODE modif id SL0.
  selection-screen end of line.

selection-screen end of screen 0950.

*----------------------------------------------------------------------
*                AT SELECTION-SCREEN
*----------------------------------------------------------------------
at selection-screen.
  perform CHECK_OUTPUT_FILE_OPTIONS.
*----------------------------------------------------------------------
*                AT SELECTION-SCREEN OUTPUT
*----------------------------------------------------------------------
at selection-screen output.

*----------------------------------------------------------------------
*                SELECTION-SCREEN VALIDATION
*----------------------------------------------------------------------
at selection-screen on P_FSTAT.
  if P_FSTAT is initial and P_INDESC <> 'Z'.
*   Basis for mandatory registers I050 and I155 (and others).
    message E001 with P_INDESC.
  endif.
  if not P_FSTAT is initial.
*    SELECT SINGLE versn FROM t011 INTO lv_versn            "1481772
*      WHERE versn = p_fstat.                               "1481772
    select single VERSN ERGAK ERGPA from T011               "1481772
      into (GV_VERSN, GV_ERGAK, GV_ERGPA)                   "1481772
      where VERSN = P_FSTAT.                                "1481772
    if SY-SUBRC <> 0.
      message E002 with P_FSTAT.
    endif.
  endif.

at selection-screen on P_INDESC.
  if P_INDESC cn 'GRB'.                                     "1378742
    message E003.
  endif.

at selection-screen on P_NIRE.
  if not P_NIRE co '0123456789 '.
    message E008.
  endif.
  if not P_NIRE is initial and
     STRLEN( P_NIRE ) <> '11'.
    message E011.
  endif.

at selection-screen on P_GJAHR.
  perform CHECK_DATE_IN_PERIOD.

at selection-screen on S_MONAT.
  perform CHECK_DATE_IN_PERIOD.

at selection-screen on block CLS.
  if P_DOCTYP is initial and not P_DTRES is initial
  or P_DTRES is initial and not P_DOCTYP is initial.
    message E005.
  endif.

at selection-screen on block ESP.
  if not P_INDESP is initial and
    ( P_DTESPI is initial and P_DTESPF is initial ).
    message E010.
  endif.
  if P_INDESP is initial and
    ( not P_DTESPI is initial or not P_DTESPF is initial ).
    message E010.
  endif.
* Initial date should be less or equal than
* final date (otherwise, no selection could occur)
  if P_DTESPI > P_DTESPF and not P_DTESPF is initial.
    message E040.
  endif.
  perform CHECK_DATE_IN_PERIOD.

at selection-screen on block OF.
  if P_PCOUT is initial and
     not P_XPCOUT is initial.
    message E004.
  endif.
  if P_APOUT is initial and
     not P_XAPOUT is initial.
    message E004.
  endif.

at selection-screen on value-request for P_PCIN.
  perform F4HELP_LOCAL_FNAME changing P_PCIN.

at selection-screen on value-request for P_APIN.
  perform F4HELP_APPLSERVER_FNAME changing P_APIN.

at selection-screen on value-request for P_PCOUT.
  perform F4HELP_LOCAL_FNAME changing P_PCOUT.

at selection-screen on value-request for P_APOUT.
  perform F4HELP_APPLSERVER_FNAME changing P_APOUT.

*&---------------------------------------------------------------------*
*&      Form  CHECK_OUTPUT_FILE_OPTIONS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form CHECK_OUTPUT_FILE_OPTIONS.

  clear: GV_APPSV, GV_LCLSV.

* Check if file should be saved in application server
  if P_XAPOUT = 'X' and not P_APOUT is initial.
    GV_APPSV = ABAP_TRUE. "Save file in application server
  endif.

* Check if file should be saved in presentation server
  if P_XPCOUT = 'X' and not P_PCOUT is initial.
    if SY-BATCH is initial.
      GV_LCLSV = ABAP_TRUE. "Save file in presentation server
    else. "validation
      message E006. "Write to Local File in Background Mode not possible
    endif.
  endif.

* At least one of the 3 options should be used
  if GV_APPSV is initial and GV_LCLSV is initial and P_TEST is initial.
    message E004. "Define an output file name
  endif.

endform.                    " CHECK_OUTPUT_FILE_OPTIONS

*&---------------------------------------------------------------------*
*&      Form  CHECK_DATE_IN_PERIOD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form CHECK_DATE_IN_PERIOD .

  data: LV_BUPER type T009B-POPER,
        LV_GJAHR type T009B-BDATJ.

  if P_DTESPI is initial and P_DTESPF is initial.
    return.
  endif.

* Select Company Code Data
  call function 'FI_COMPANY_CODE_DATA'
    exporting
      I_BUKRS      = P_BUKRS
    importing
      E_T001       = GS_T001
    exceptions
      SYSTEM_ERROR = 0
      others       = 0.

* Select Fiscal Year Variant of non-leading ledger         "1410166
  if not S_RLDNR is initial.                                "1410166
    call function 'FAGL_GET_INFO_FROM_LEDGER'               "1410166
      exporting                                             "1410166
*       I_BUDAT        =
        I_RLDNR        = S_RLDNR-LOW                  "1410166
        I_BUKRS        = P_BUKRS                      "1410166
      importing                                             "1410166
        E_PERIV        = GV_RLDNR_PERIV               "1410166
*       E_POPER        =
*       E_GJAHR        =
      exceptions                                            "1410166
        NO_INFO_FOUND  = 1                            "1410166
        ERROR_IN_SETUP = 2                            "1410166
        others         = 3.                           "1410166
    if SY-SUBRC <> 0.
      GV_RLDNR_PERIV = GS_T001-PERIV.                       "1410166
    endif.
  else.
    GV_RLDNR_PERIV = GS_T001-PERIV.                         "1410166
  endif.                                                    "1410166

* Check initial date occurs inside period range selected
  if not P_DTESPI is initial.
    clear: LV_BUPER, LV_GJAHR.
    call function 'DATE_TO_PERIOD_CONVERT'
      exporting
        I_DATE         = P_DTESPI
*       I_MONMIT       = 00
        I_PERIV        = GV_RLDNR_PERIV                 "1410166
      importing
        E_BUPER        = LV_BUPER
        E_GJAHR        = LV_GJAHR
      exceptions
        INPUT_FALSE    = 0
        T009_NOTFOUND  = 0
        T009B_NOTFOUND = 0
        others         = 0.
    if LV_GJAHR <> P_GJAHR or not LV_BUPER in S_MONAT.
      message E040.
    endif.
  endif.

* Check final date occurs inside period range selected
  if not P_DTESPF is initial.
    clear: LV_BUPER, LV_GJAHR.
    call function 'DATE_TO_PERIOD_CONVERT'
      exporting
        I_DATE         = P_DTESPF
*       I_MONMIT       = 00
        I_PERIV        = GV_RLDNR_PERIV                 "1410166
      importing
        E_BUPER        = LV_BUPER
        E_GJAHR        = LV_GJAHR
      exceptions
        INPUT_FALSE    = 0
        T009_NOTFOUND  = 0
        T009B_NOTFOUND = 0
        others         = 0.
    if LV_GJAHR <> P_GJAHR or not LV_BUPER in S_MONAT.
      message E040.
    endif.
  endif.

endform.                    " CHECK_DATE_IN_PERIOD

at selection-screen on P_LEDGER.                            "1378742
  if P_INDESC = 'B' and                                     "1378742
     not P_LEDGER is initial.                               "1378742
    perform CHECK_DAY_LEDGER.                               "1378742
  endif.                                                    "1378742
*&---------------------------------------------------------------------*
*&      Form  CHECK_DAY_LEDGER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form CHECK_DAY_LEDGER .                                     "1378742

  data: LS_ORG_INFO like GLX_ORG_INFO,                      "1378742
        LV_ANZBP    like T009-ANZBP,                        "1378742
        LV_ANZSP    like T009-ANZSP.                        "1378742

* determination of number of periods                        "1378742
  call function 'FI_PERIOD_INFO_GET'                        "1378742
    exporting                                               "1378742
      I_BUKRS = P_BUKRS                               "1378742
      I_RLDNR = P_LEDGER                              "1378742
    importing                                               "1378742
      E_ANZBP = LV_ANZBP                              "1378742
      E_ANZSP = LV_ANZSP.                             "1378742

  if LV_ANZBP <> '365' and                                  "1378742
     LV_ANZBP <> '366'.                                     "1378742
* Special Ledger is no day ledger                           "1378742
    message E043 with P_LEDGER.                             "1378742
  endif.                                                    "1378742
endform.                    " CHECK_DAY_LEDGER              "1378742

at selection-screen on P_INDEGP.                            "1961806
  if ( P_LAYOUT = '002' or P_LAYOUT = '003' )               "2154499
    and ( ( P_INDEGP <> '0' and P_INDEGP <> '1' )           "1961806
    or P_INDEGP is initial ).                               "1961806
    message E046.                                           "1961806
  endif.                                                    "1961806

  if P_LAYOUT = '001'                                       "1961806
    and P_INDEGP is not initial.                            "1961806
    message E047.                                           "1961806
  endif.                                                    "1961806

* initialization
initialization.
  include J_1BECD_MAIN_INI.

  data LS_VERSIONS like line of GT_VERSIONS.
  "substitui estrutura J150 versao 003
  delete GT_VERSIONS where REGISTER  = 'J150'
                     and   LAYOUT    = '003'.

  clear LS_VERSIONS.
  LS_VERSIONS-REGISTER = 'J150'.
  LS_VERSIONS-LAYOUT   = '003'.
  LS_VERSIONS-TYPE     = 'J_1BECD_J150_3_S_003'.
  append LS_VERSIONS to GT_VERSIONS.

  delete GT_VERSIONS where REGISTER  = 'J930'
                     and   LAYOUT    = '006'.

  clear LS_VERSIONS.
  LS_VERSIONS-REGISTER = 'J930'.
  LS_VERSIONS-LAYOUT   = '006'.
  LS_VERSIONS-TYPE     = 'J_1BECD_J930_3_S_006'.
  append LS_VERSIONS to GT_VERSIONS.

start-of-selection.

  if GV_APPSV = ABAP_TRUE.
    perform OPEN_FILE using 0.
  endif.

  perform: F_SELECIONA_DADOS,
           F_BLOCO_0,
           F_BLOCO_I,
           FILE_DOWNLOAD.
*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form F_SELECIONA_DADOS .

  data: LV_TEXT   type VAL_TEXT,
        LS_FSV    type TP_FSV,
        VBELNR    type BSEG-BELNR,
        VQTDE(10),
        VFIM(1),
        VPROC(1).

  data LS_SKC1A    type SKC1A.
  write: / 'Inicio Leitura' .
  write: SY-UZEIT.


  select BUKRS BELNR GJAHR BUDAT BLART XBLNR
    from BKPF
  into table T_BKPF
  where BUKRS = P_BUKRS and
        GJAHR = P_GJAHR and
        MONAT  between S_MONAT-LOW and S_MONAT-HIGH and
        ( RLDNR in ('' ,S_RLDNR-LOW) or RLDNR is null ) and
        ( LDGRP in ('' ,S_RLDNR-LOW) or LDGRP is null ) and
        BSTAT = SPACE.

  wait up to 1 seconds.
  write: / 'Leitura BKPF' .
  write: SY-UZEIT.


  refresh T_BSEG.
  if T_BKPF[] is not initial.
    sort T_BKPF by BUKRS BELNR GJAHR.
    select BUKRS BELNR GJAHR BUZEI SHKZG DMBE2 HKONT DMBTR SGTXT XBILK VBUND
          from BSEG
         into table T_BSEG
     for all entries in T_BKPF
      where BSEG~BUKRS = T_BKPF-BUKRS and
            BSEG~BELNR = T_BKPF-BELNR and
            BSEG~GJAHR = T_BKPF-GJAHR.
    wait up to 1 seconds.
    write: / 'Leitura BSEG' .
    write: SY-UZEIT.
    if R_MOEDA = 'X'.
      delete  T_BSEG where DMBTR = 0 and DMBE2 = 0.
    else.
      delete  T_BSEG where DMBTR = 0.
    endif.
    sort  T_BSEG by BUKRS BELNR GJAHR.
  endif.

  "alrs

* Select Company Code Data
  call function 'FI_COMPANY_CODE_DATA'
    exporting
      I_BUKRS      = P_BUKRS
    importing
      E_T001       = GS_T001
    exceptions
      SYSTEM_ERROR = 0
      others       = 0.

  data:
    LT_CALLBACK   type table of LDBCB,
    LT_SELTAB     type table of RSPARAMS,
    LS_SELTAB     like line of LT_SELTAB,
    LV_VAL        type TVARV_VAL,
    LT_FIELDS     type RSFS_FIELDS,
    LS_TAB_FIELDS type RSFS_TAB_FIELDS,
    LS_STRUC      type RSFS_STRUC,
    LV_PLAN       type I.

  refresh:
* Account table is always filled
   GT_SKA1,  "SDF account selection result table
* Balance table is filled only when Total Balances are needed
   GTS_SKC1A. "SDF balance selection result table

  if P_YECP = ABAP_TRUE.                                    "2040307
    LV_PLAN = 2.                                            "2040307
  else.                                                     "2040307
    clear LV_PLAN.                                          "2040307
  endif.                                                    "2040307

* LDB selection options
  GV_REPID = SY-REPID.
  LV_VAL = GS_T001-KTOPL.
  perform FILL_SELTAB using 'SD_KTOPL' 'S' 'I' 'EQ' LV_VAL SPACE
                   changing LT_SELTAB[].
  LV_VAL = P_BUKRS.
  perform FILL_SELTAB using 'SD_BUKRS' 'S' 'I' 'EQ' LV_VAL SPACE
                   changing LT_SELTAB[].
  LV_VAL = P_GJAHR - 1. "ALRS
  perform FILL_SELTAB using 'SD_GJAHR' 'S' 'I' 'EQ' LV_VAL SPACE
                   changing LT_SELTAB[].
  LV_VAL = LV_PLAN.                                         "2040307
  condense LV_VAL no-gaps.                                  "2040307
  perform FILL_SELTAB using 'SD_PLAN' 'S' 'I' 'EQ' LV_VAL SPACE "2040307
                   changing LT_SELTAB[].                    "2040307
*  lv_val = iv_rldnr.                                             "1618302
*  PERFORM fill_seltab USING 'SD_RLDNR' 'S' 'I' 'EQ' lv_val space "1618302
*                   CHANGING lt_seltab[].                         "1618302
  if not S_RLDNR is initial.                                "1618302
    LV_VAL = S_RLDNR-LOW.                                   "1618302
    perform FILL_SELTAB                                     "1618302
         using 'SD_RLDNR' 'S' 'I' 'EQ' LV_VAL SPACE         "1618302
      changing LT_SELTAB[].                                 "1618302
  endif.                                                    "1618302

* Filter using G/L Account Number customer input
  LS_SELTAB-KIND = 'S'.
  LS_SELTAB-SELNAME = 'SD_SAKNR'.
  loop at S_SAKNR into S_SAKNR.
    move-corresponding S_SAKNR to LS_SELTAB.
    append LS_SELTAB to LT_SELTAB.
  endloop.
* LDB needed nodes and callback form
  perform FILL_CALLBACK using 'SKA1'  'X' 'X' 'CALLBACK_SDF_SKA1'
                     changing LT_CALLBACK[].
  perform FILL_CALLBACK using 'SKB1'  'X' ' ' 'CALLBACK_SDF_SKB1'
                     changing LT_CALLBACK[].


  perform FILL_CALLBACK using 'SKC1A' 'X' ' ' 'CALLBACK_SDF_SKC1A'
                     changing LT_CALLBACK[].


*  FIELD RESTRICTION (FIELDS TO BE SELECTED)
  clear LS_TAB_FIELDS.
  refresh LS_TAB_FIELDS-FIELDS.
  LS_TAB_FIELDS-TABLENAME = 'SKA1'.
  LS_STRUC-LINE           = 'SAKNR'.
  append LS_STRUC-LINE to LS_TAB_FIELDS-FIELDS.
  LS_STRUC-LINE           = 'ERDAT'.
  append LS_STRUC-LINE to LS_TAB_FIELDS-FIELDS.
  append LS_TAB_FIELDS to LT_FIELDS.

  clear LS_TAB_FIELDS.
  refresh LS_TAB_FIELDS-FIELDS.
  LS_TAB_FIELDS-TABLENAME = 'SKC1A'.
  LS_STRUC-LINE           = '*'.
  append LS_STRUC-LINE to LS_TAB_FIELDS-FIELDS.
  append LS_TAB_FIELDS to LT_FIELDS.

* Double check this authorization, as LDB call would result in
* sy-subrc=6, which could not be easily interpreted by customer
  authority-check object 'F_BKPF_KOA'
    id 'KOART' field 'S'
    id 'ACTVT' field '03'.
  if SY-SUBRC <> 0.
    message E812(FR) with text-034 'S'.                     "#EC *
*   message from SAPDBSDF, TEXT-008
  endif.

  clear: SY-MSGNO.
  call function 'LDB_PROCESS'
    exporting
      LDBNAME                     = 'SDF'
      FIELD_SELECTION             = LT_FIELDS
    tables
      CALLBACK                    = LT_CALLBACK
      SELECTIONS                  = LT_SELTAB
    exceptions
      LDB_NOT_REENTRANT           = 1
      LDB_INCORRECT               = 2
      LDB_ALREADY_RUNNING         = 3
      LDB_ERROR                   = 4
      LDB_SELECTIONS_ERROR        = 5
      LDB_SELECTIONS_NOT_ACCEPTED = 6
      VARIANT_NOT_EXISTENT        = 7
      VARIANT_OBSOLETE            = 8
      VARIANT_ERROR               = 9
      FREE_SELECTIONS_ERROR       = 10
      CALLBACK_NO_EVENT           = 11
      CALLBACK_NODE_DUPLICATE     = 12
      CALLBACK_NO_PROGRAM         = 13
      CALLBACK_NO_CBFORM          = 14
      DYN_NODE_NO_TYPE            = 15
      DYN_NODE_INVALID_TYPE       = 16
      others                      = 17.
  if SY-SUBRC <> 0.
    if SY-MSGNO is initial. "LDB_PROCESS may issue message
      clear LV_TEXT.
      message E030 into LV_TEXT with SY-SUBRC 'SDF'.
    endif.
    if not LV_TEXT is initial.
      call function 'J_1BECD_LOG_ADD_MESSAGE'
        exporting
          IV_EXCNO = GV_EXCNO.
    endif.
  endif.

  wait up to 1 seconds.
  write: / 'Leitura Plano STD' .
  write: SY-UZEIT.


* c) Select Financial Statement Version
  perform SELECT_AND_BUFFER_FSV using P_FSTAT
                                      GS_T001-KTOPL
                                      P_SPRA.
  wait up to 1 seconds.
  write: / 'Leitura Estrutura STD' .
  write: SY-UZEIT.


  loop at GT_FSV into LS_FSV.
    move-corresponding LS_FSV to W_AGL.
    append W_AGL to T_AGL.
    if W_AGL-IS_ANALY_SINT = 'A'.
      append W_AGL to T_AGL_A.
    endif.
  endloop.

  sort T_AGL_A by ACCOUNT_NR.
  clear VBELNR.
  loop at T_BSEG into W_BSEG.
    TABIX = SY-TABIX.
    if VBELNR = W_BSEG-BELNR.
      W_BSEG-DEL = 'X'.
      modify T_BSEG from W_BSEG index TABIX transporting DEL.
      continue.
    endif.
    LV_ACCOUNT_NR = W_BSEG-HKONT.
    read table T_AGL_A transporting no fields
          with key ACCOUNT_NR = LV_ACCOUNT_NR  binary search.

    if SY-SUBRC ne 0.
      VBELNR =  W_BSEG-BELNR.
      W_BSEG-DEL = 'X'.
      W_DEL-HKONT = W_BSEG-HKONT.
      W_DEL-QTD   = 1.
      collect W_DEL into T_DEL.
      modify T_BSEG from W_BSEG index TABIX transporting DEL.
      continue.
    endif.

    clear VBELNR.

    read table  T_BKPF into W_BKPF with key BUKRS = W_BSEG-BUKRS
                                BELNR = W_BSEG-BELNR
                                GJAHR = W_BSEG-GJAHR binary search.

    W_BSEG-BUDAT = W_BKPF-BUDAT.
    modify T_BSEG from W_BSEG index TABIX transporting BUDAT.

    "Saldo conta
    W_SLD-MES    = W_BKPF-BUDAT+4(2).
    W_SLD-HKONT  = W_BSEG-HKONT.
    clear: W_SLD-VL_DEB, W_SLD-VL_CRE,W_SLD-VL_DEBF, W_SLD-VL_CREF.
    if W_BSEG-SHKZG = 'S'.
      W_SLD-VL_DEB = W_BSEG-DMBTR.
      W_SLD-VL_DEBF = W_BSEG-DMBE2.
    else.
      W_SLD-VL_CRE = W_BSEG-DMBTR.
      W_SLD-VL_CREF = W_BSEG-DMBE2.
    endif.
    collect W_SLD into T_SLD.

    "valor documento
    if W_BSEG-SHKZG = 'H'.
      W_DOC-BUKRS = W_BSEG-BUKRS.
      W_DOC-BELNR = W_BSEG-BELNR.
      W_DOC-GJAHR = W_BSEG-GJAHR.
      W_DOC-BUDAT = W_BKPF-BUDAT.
      W_DOC-TOTAL = W_BSEG-DMBTR.
      W_DOC-TOTALF = W_BSEG-DMBE2.
      W_DOC-XBLNR = W_BKPF-XBLNR.
      if W_BKPF-BLART = P_DOCTYP.
        W_DOC-TIPO = 'E'.
      else.
        W_DOC-TIPO = 'N'.
      endif.
      collect W_DOC into T_DOC.
    endif.

    if W_BKPF-BLART = P_DOCTYP.
      W_ENC-HKONT = W_BSEG-HKONT.
      W_ENC-TOTAL = W_BSEG-DMBTR.
      W_ENC-TOTALF = W_BSEG-DMBE2.
      if W_BSEG-XBILK = 'X'.
        if W_BSEG-SHKZG = 'S'.
          multiply W_ENC-TOTAL by -1.
          multiply W_ENC-TOTALF by -1.
        endif.
      else.
        if W_BSEG-SHKZG = 'H'.
          multiply W_ENC-TOTAL by -1.
          multiply W_ENC-TOTALF by -1.
        endif.
      endif.
      collect W_ENC into T_ENC.
    endif.

  endloop.
  delete T_BSEG where DEL = 'X'.
  sort T_DEL by HKONT.
  loop at T_DEL into W_DEL.
    VQTDE =  W_DEL-QTD.
    write: / 'Conta não está na estrutura'.
    write W_DEL-HKONT.
    write VQTDE.
  endloop.
  free: T_BKPF.

  refresh: IT_SALDO_CONTAS,IT_SALDO_CONTAS_2, IT_SALDO_CONTAS_3, IT_CONTAS.

  WA_CONTAS-BUKRS = P_BUKRS.
  WA_CONTAS-SAKNR = '*'.
  append WA_CONTAS to IT_CONTAS.
  "
  call function 'Z_FI_GL_SALDO_FAGLFLEXT'
    exporting
      RYEAR         = P_GJAHR
      CONTAS        = IT_CONTAS
      P_GERAR_TODAS = 'X'
    tables
      IT_SALDOS     = IT_SALDO_CONTAS
      IT_SALDOS_2   = IT_SALDO_CONTAS_2
    exceptions
      MOEDA_NAO_ADM = 1      "IT_SALDOS_3 = IT_SALDO_CONTAS_3
      ERRO_LEDGER   = 2
      others        = 3.

  if SY-SUBRC <> 0.
    message id SY-MSGID type SY-MSGTY number SY-MSGNO with SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  endif.

  "saldo estrutura
  sort IT_SALDO_CONTAS by RACCT.
  sort IT_SALDO_CONTAS_2 by RACCT.
  if S_MONAT-HIGH = 16.
    VMES = 16.
    VMES_INI = S_MONAT-LOW - 1.
    VMES2 = 15.
  else.
    VMES = S_MONAT-HIGH.
    VMES2 = S_MONAT-HIGH.
    VMES_INI = 0.
  endif.

  loop at T_AGL_A into W_AGL_A.
    TABIX = SY-TABIX.
    read table IT_SALDO_CONTAS with key RACCT = W_AGL_A-ACCOUNT_NR binary search.
    if SY-SUBRC = 0.
      WL_SALDO_MI2 = 0.

      W_AGL_A-TOTAL_INI = IT_SALDO_CONTAS-SLVT.

      "Arquivo dividido
      if VMES_INI ne 0.
        do VMES_INI times varying REFE1 from IT_SALDO_CONTAS-SL01 next IT_SALDO_CONTAS-SL02.
          add REFE1 to W_AGL_A-TOTAL_INI.
        enddo.
      endif.

      add IT_SALDO_CONTAS-SLVT to WL_SALDO_MI2.
      do VMES times varying REFE1 from IT_SALDO_CONTAS-SL01 next IT_SALDO_CONTAS-SL02.
        add REFE1 to WL_SALDO_MI2.
      enddo.
      W_AGL_A-TOTAL_FIN = WL_SALDO_MI2.
      "16
      WL_SALDO_MI2 = 0.
      add IT_SALDO_CONTAS-SLVT to WL_SALDO_MI2.
      do VMES2 times varying REFE1 from IT_SALDO_CONTAS-SL01 next IT_SALDO_CONTAS-SL02.
        add REFE1 to WL_SALDO_MI2.
      enddo.
      W_AGL_A-TOTAL = WL_SALDO_MI2.
      "
      if R_MOEDA = 'X'.
        read table IT_SALDO_CONTAS_2 with key RACCT = W_AGL_A-ACCOUNT_NR binary search.
        if SY-SUBRC = 0.
          WL_SALDO_MI2 = 0.

          W_AGL_A-TOTAL_INI_F = IT_SALDO_CONTAS_2-SLVT.

          "Arquivo dividido
          if VMES_INI ne 0.
            do VMES_INI times varying REFE1 from IT_SALDO_CONTAS_2-SL01 next IT_SALDO_CONTAS_2-SL02.
              add REFE1 to W_AGL_A-TOTAL_INI_F.
            enddo.
          endif.

          add IT_SALDO_CONTAS_2-SLVT to WL_SALDO_MI2.
          do VMES times varying REFE1 from IT_SALDO_CONTAS_2-SL01 next IT_SALDO_CONTAS_2-SL02.
            add REFE1 to WL_SALDO_MI2.
          enddo.
          W_AGL_A-TOTAL_FIN_F = WL_SALDO_MI2.
        endif.
        WL_SALDO_MI2 = 0.
        add IT_SALDO_CONTAS_2-SLVT to WL_SALDO_MI2.
        do VMES2 times varying REFE1 from IT_SALDO_CONTAS_2-SL01 next IT_SALDO_CONTAS_2-SL02.
          add REFE1 to WL_SALDO_MI2.
        enddo.
        W_AGL_A-TOTALF = WL_SALDO_MI2.
      endif.
      "
      read table GTS_SKC1A into LS_SKC1A
         with key MANDT = SY-MANDT
                  BUKRS = P_BUKRS
                  SAKNR = W_AGL_A-ACCOUNT_NR.
      if SY-SUBRC = 0.
        W_AGL_A-TOTAL_PER_A = LS_SKC1A-UM15K.
      endif.

      modify T_AGL_A from W_AGL_A index TABIX transporting TOTAL_INI TOTAL_FIN TOTAL TOTAL_INI_F TOTAL_FIN_F TOTALF TOTAL_PER_A.
    else.
      read table GTS_SKC1A into LS_SKC1A
      with key MANDT = SY-MANDT
               BUKRS = P_BUKRS
               SAKNR = W_AGL_A-ACCOUNT_NR.
      if SY-SUBRC = 0.
        W_AGL_A-TOTAL_PER_A = LS_SKC1A-UM15K.
      endif.

      modify T_AGL_A from W_AGL_A index TABIX transporting TOTAL_PER_A.
    endif.
  endloop.

  loop at T_AGL_A into W_AGL_A.
    clear VPROC.

    read table T_AGL into W_AGL with key  ACCOUNT_NR    = W_AGL_A-ACCOUNT_NR
                                          IS_ANALY_SINT = 'A'.
    if SY-SUBRC = 0.
      W_AGL-TOTAL_INI = W_AGL_A-TOTAL_INI.
      W_AGL-TOTAL_FIN = W_AGL_A-TOTAL_FIN.
      W_AGL-TOTAL     = W_AGL_A-TOTAL.
      "
      W_AGL-TOTAL_INI_F = W_AGL_A-TOTAL_INI_F.
      W_AGL-TOTAL_FIN_F = W_AGL_A-TOTAL_FIN_F.
      W_AGL-TOTALF      = W_AGL_A-TOTALF.
      W_AGL-TOTAL_PER_A = W_AGL_A-TOTAL_PER_A.
      "
      collect W_AGL into T_AGL.
      clear VFIM.
      while VFIM = ''.
        read table T_AGL into W_AGL2 with key  ACCOUNT_NR    = W_AGL-SUPERIOR_ACC_NR.
        if SY-SUBRC = 0.
          if W_AGL-SUPERIOR_ACC_NR is initial.
            VFIM = 'X'.
            continue.
          endif.
          W_AGL2-TOTAL_INI = W_AGL_A-TOTAL_INI.
          W_AGL2-TOTAL_FIN = W_AGL_A-TOTAL_FIN.
          W_AGL2-TOTAL     = W_AGL_A-TOTAL.
          "
          W_AGL2-TOTAL_INI_F = W_AGL_A-TOTAL_INI_F.
          W_AGL2-TOTAL_FIN_F = W_AGL_A-TOTAL_FIN_F.
          W_AGL2-TOTALF      = W_AGL_A-TOTALF.
          "
          W_AGL2-TOTAL_PER_A = W_AGL_A-TOTAL_PER_A.
          collect W_AGL2 into T_AGL.
          W_AGL-SUPERIOR_ACC_NR =  W_AGL2-SUPERIOR_ACC_NR.
        else.
          VFIM = 'X'.
        endif.
      endwhile.
    endif.
  endloop.

  wait up to 1 seconds.
  write: / 'Totalização' .
  write: SY-UZEIT.

  sort: T_SLD  by MES HKONT,
        T_ENC  by HKONT,
        T_DOC  by BUKRS BELNR GJAHR,
        T_BSEG by BUDAT BUKRS BELNR GJAHR BUZEI.


endform.                    " F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*&      Form  F_BLOCO_I
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form F_BLOCO_I .

  data GS_I051_N type J_1BECD_I051_4_S_003.
  data: LS_J100 type J_1BECD_J100_3_S,
        LS_J150 type J_1BECD_J150_3_S.


  GS_I001-REG     = 'I001'.
  GS_I001-IND_DAD = CONST_BLOCK_WITH. "by definition for ECD
  append GS_I001 to GT_I001.
  perform FORMAT_RECORD_TO_LINE using GS_I001
                                  changing LS_RESULT.       "1909036
  if GV_APPSV = ABAP_TRUE.
    transfer LS_RESULT to P_APOUT.
  else.
    GS_RESULT = LS_RESULT.
    append GS_RESULT to GT_RESULT.
  endif.
  wait up to 1 seconds.
  write: / 'I001' .
  write: SY-UZEIT.


  "I010
* Bookkeeping Identification - Cardinality 1
  data: LS_I010  type J_1BECD_I010_2_S.
  data: LV_NAME  type DOMNAME,
        LV_VALUE type DOMVALUE_L,
        LV_TEXT  type VAL_TEXT.


  LV_NAME         = 'J_1BECD_COD_VER'.
  LV_VALUE        = P_LAYOUT.
  perform GET_DOMAIN_TEXT using LV_NAME
                                LV_VALUE
                       changing LV_TEXT.

  clear LS_I010.
  LS_I010-REG        = 'I010'.
  LS_I010-IND_ESC    = P_INDESC.
  LS_I010-COD_VER_LC = LV_TEXT.
  clear GT_I010.
  append LS_I010 to GT_I010.
  perform FORMAT_RECORD_TO_LINE using LS_I010
                                 changing LS_RESULT.        "1909036
  if GV_APPSV = ABAP_TRUE.
    transfer LS_RESULT to P_APOUT.
  else.
    GS_RESULT = LS_RESULT.
    append GS_RESULT to GT_RESULT.
  endif.
  wait up to 1 seconds.
  write: / 'I010' .
  write: SY-UZEIT.

  "I020
  if R_MOEDA = 'X'.
    do 11 times.
      case SY-INDEX.
        when 1.
          LS_RESULT = '|I020|I155|10|VL_SLD_INI_AUX|valor saldo inicial moeda funcional|N|'.
        when 2.
          LS_RESULT = '|I020|I155|11|IND_DC_INI_AUX|indicador da situacao|C|'.
        when 3.
          LS_RESULT = '|I020|I155|12|VL_DEB_AUX|valor total dos debitos moeda funcional|N|'.
        when 4.
          LS_RESULT = '|I020|I155|13|VL_CRED_AUX|valor total dos credito moeda funcional|N|'.
        when 5.
          LS_RESULT = '|I020|I155|14|VL_SLD_FIN_AUX|valor saldo final moeda funcional|N|'.
        when 6.
          LS_RESULT = '|I020|I155|15|IND_DC_FIN_AUX|indicador da situacao|C|'.
        when 7.
          LS_RESULT = '|I020|I200|06|VL_LCTO_AUX|valor lançamento moeda funcional|N|'.
        when 8.
          LS_RESULT = '|I020|I250|10|VL_DC_AUX|valor da partida moeda funcional|N|'.
        when 9.
          LS_RESULT = '|I020|I250|11|IND_DC_AUX|indicador da natureza da partida|C|'.
        when 10.
          LS_RESULT = '|I020|I355|06|VL_CTA_AUX|Valor saldo final moeda funcional|N|'.
        when 11.
          LS_RESULT = '|I020|I355|07|IND_DC_AUX|indicador da situacao|C|'.
      endcase.
      W_CONTADOR-REG_BLC     = LS_RESULT+1(4).
      W_CONTADOR-QTD_REG_BLC = 1.
      collect  W_CONTADOR into T_CONTADOR.
      add 1 to W_TOTAL_J900.
      add 1 to W_TOTAL_I990.

      if GV_APPSV = ABAP_TRUE.
        transfer LS_RESULT to P_APOUT.
      else.
        GS_RESULT = LS_RESULT.
        append GS_RESULT to GT_RESULT.
      endif.
    enddo.
  endif.


* Journal Opening Term - Cardinality 1
  data: LS_I030    type J_1BECD_I030_3_S,
        LV_COUNTER type NUM10,
        IV_IDX     type I,
        W_ERDAT    type SKB1-ERDAT,
        W_SAKNR    type SKB1-SAKNR.

  clear LS_I030.
  LS_I030-REG            = 'I030'.
  LS_I030-DNRC_ABERT     = text-902.
  LS_I030-NUM_ORD        = P_NUMORD.
  LS_I030-NAT_LIVR       = P_NATLIV.
* total file record counter filled at end, but need to be initialized
  LS_I030-QTD_LIN        = '0000000000'.
  if not GV_NOME is initial.
    LS_I030-NOME           = GV_NOME.
  else.
    LS_I030-NOME           = GS_BRANCH_DATA-NAME.
  endif.
  LS_I030-NIRE           = P_NIRE.
  LS_I030-CNPJ           = GV_CNPJ. "gs_branch_data-cgc_branch.
  LS_I030-DT_ARQ         = P_DTARQ.
  LS_I030-DT_ARQ_CONV    = P_DTCONV.
  LS_I030-DESC_MUN       = GS_ADRC-CITY1. "gs_t001-ort01.
  LS_I030-DT_EX_SOCIAL   = P_PERCLS.                        "1909036

  clear GT_I030.
  append LS_I030 to GT_I030.
  GS_SAVED_I030 = LS_I030.
  perform FORMAT_RECORD_TO_LINE using LS_I030
                                 changing LS_RESULT.        "1909036
  clear GV_OFFSET_I030.
  if GV_APPSV = ABAP_TRUE.
    get dataset P_APOUT position GV_OFFSET_I030.
  endif.

  if GV_APPSV = ABAP_TRUE.
    transfer LS_RESULT to P_APOUT.
  else.
    GS_RESULT = LS_RESULT.
    append GS_RESULT to GT_RESULT.
  endif.
  GV_INDEX_I030 = W_TOTAL_J900.

  wait up to 1 seconds.
  write: / 'I030' .
  write: SY-UZEIT.


  "I050
* CHART OF ACCOUNTS - CARDINALITY N
  data: LS_I050          type J_1BECD_I050_3_S,
        LS_FSV           like line of GT_FSV,
        LS_I052          type J_1BECD_I052_4_S,
        LS_ACCOUNT_LEVEL type J_1BECD_SINTETIC_ACC_S.

* BAdI instance
* Single implementation BAdI - no default imlementation provided
  try.
      get badi GREF_BADI_J_1BECD.
    catch: CX_BADI_NOT_IMPLEMENTED into GR_OREF.        "#EC NO_HANDLER
  endtry.

  W_CONTADOR-REG_BLC     = 'I050'.
  W_CONTADOR-QTD_REG_BLC = 0.
  collect  W_CONTADOR into T_CONTADOR.
  W_CONTADOR-REG_BLC     = 'I051'.
  W_CONTADOR-QTD_REG_BLC = 0.
  collect  W_CONTADOR into T_CONTADOR.

  loop at GT_FSV into LS_FSV.

    if SY-TABIX = 1.
      refresh:
* only a few records at a time: current higher nodes in the hierarchy
        GT_ACCOUNT_LEVEL,                                     "(I052)
* grow with the i050 processing: all aglut.codes x analytical accounts
* and to be kept until J block
        GT_BAL_AGLUT_LEVEL, "for balance (J100)
        GT_PL_AGLUT_LEVEL.  "for profit & loss statement (J150)
    endif.

* Global Level control
* At each moment it contains the higher nodes as processed before plus
* the new refreshed nodes below
* Example: 1-2-3-4-2 (at this point level 2, 3 and 4 nodes are deleted)
* and level 2 is inserted with fresh info. Node 1 remains as it is still
* the valid top node in this hierarchy
    if LS_FSV-IS_ANALY_SINT = CONST_ACC_SINTETIC.
      delete GT_ACCOUNT_LEVEL where FSV_LEVEL >= LS_FSV-LEVEL.
      LS_ACCOUNT_LEVEL-FSV_LEVEL   = LS_FSV-LEVEL.
      if P_ALTKT = 'X' and LS_FSV-ALTKT is not initial.     "1408160
* alternative account number                                "1408160
        LS_ACCOUNT_LEVEL-ACCOUNT_NR  = LS_FSV-ALTKT.        "1408160
      else.                                                 "1408160
        LS_ACCOUNT_LEVEL-ACCOUNT_NR  = LS_FSV-ACCOUNT_NR.   "1408160
      endif.                                                "1408160
      LS_ACCOUNT_LEVEL-DESCRIPTION = LS_FSV-DESCRIPTION.
      append LS_ACCOUNT_LEVEL to GT_ACCOUNT_LEVEL.
    endif.

    clear GT_I050.    "reposition of this line                "1481772

    clear LS_I050.
    LS_I050-REG         = 'I050'.
    LS_I050-DT_ALT      = LS_FSV-INCLUSION_DATE.
    LS_I050-COD_NAT     = LS_FSV-SPED_INT_CODE.
    LS_I050-IND_CTA     = LS_FSV-IS_ANALY_SINT.
    LS_I050-NIVEL       = LS_FSV-LEVEL.
    if P_ALTKT = 'X' and LS_FSV-ALTKT is not initial.       "1408160
      LS_I050-COD_CTA     = LS_FSV-ALTKT.                   "1408160
    else.                                                   "1408160
      LS_I050-COD_CTA     = LS_FSV-ACCOUNT_NR.              "1408160
    endif.                                                  "1408160
    LS_I050-COD_CTA_SUP = LS_FSV-SUPERIOR_ACC_NR.
    LS_I050-CTA         = LS_FSV-DESCRIPTION.

    if LS_I050-DT_ALT is initial or
     LS_I050-DT_ALT eq '00000000'.
      W_SAKNR = LS_I050-COD_CTA(10).
      select single ERDAT from SKB1
        into W_ERDAT
        where BUKRS = P_BUKRS and
              SAKNR = W_SAKNR.
      if SY-SUBRC = 0.
        LS_I050-DT_ALT = W_ERDAT.
      else.
        LS_I050-DT_ALT  = GS_FIN_CONTROL-DTINI. "convention
      endif.
    endif.

* Process line for all cases, except the special P&L cases  "1481772
    if LS_FSV-IS_ANALY_SINT = CONST_ACC_SPEC_LOSS or        "1481772
       LS_FSV-IS_ANALY_SINT = CONST_ACC_SPEC_PROFIT.        "1481772
      perform FILL_PL_AGLUTINATION using LS_I050.           "1481772
    else.  "general case                                      "1481772
*      APPEND LS_I050 TO GT_I050.
    endif.
    perform FORMAT_RECORD_TO_LINE using LS_I050
                                  changing LS_RESULT.       "1909036
    if GV_APPSV = ABAP_TRUE.
      transfer LS_RESULT to P_APOUT.
    else.
      GS_RESULT = LS_RESULT.
      append GS_RESULT to GT_RESULT.
    endif.
    "I051 nested
    clear GT_I051.
    if LS_I050-IND_CTA = CONST_ACC_ANALYTICAL.
      call badi GREF_BADI_J_1BECD->FILL_REGISTER_I051
        exporting
          IV_BUKRS    = P_BUKRS
          IS_REG_I050 = LS_I050
        importing
          ET_REG_I051 = GT_I051.
      if GT_I051[] is initial.
        write: / 'Conta ref. não encontrada'.
        write LS_I050-COD_CTA+0(15).
      endif.
      loop at GT_I051 into GS_I051.
        move-corresponding GS_I051 to GS_I051_N.
        GS_I051_N-COD_PLAN_REF = GS_I051-COD_ENT_REF.
        perform FORMAT_RECORD_TO_LINE using GS_I051_N
                              changing LS_RESULT.           "1909036
        if GV_APPSV = ABAP_TRUE.
          transfer LS_RESULT to P_APOUT.
        else.
          GS_RESULT = LS_RESULT.
          append GS_RESULT to GT_RESULT.
        endif.
      endloop.
* Aglutination code is the FSV level code
      loop at GT_ACCOUNT_LEVEL into LS_ACCOUNT_LEVEL.
        LS_I052-REG     = 'I052'.
        LS_I052-COD_AGL = LS_ACCOUNT_LEVEL-ACCOUNT_NR.
*        APPEND LS_I052 TO GT_I052.
        perform FORMAT_RECORD_TO_LINE using LS_I052
                            changing LS_RESULT.             "1909036
        if GV_APPSV = ABAP_TRUE.
          transfer LS_RESULT to P_APOUT.
        else.
          GS_RESULT = LS_RESULT.
          append GS_RESULT to GT_RESULT.
        endif.
      endloop.
    endif.
  endloop.
  wait up to 1 seconds.
  write: / 'I050' .
  write: SY-UZEIT.


  "I100
  data: LS_I100 type J_1BECD_I100_3_S.
  data: LT_I100 type J_1BECD_I100_3_T.
  data: LT_CSKS_EX type table of CSKS_EX.
  data: LV_COD_CCUS_ANT type KOSTL.

  field-symbols: <LV_CSKS_EX> type CSKS_EX.

  call function 'KOKRS_GET_FROM_BUKRS' "Get Company Controlling Area
    exporting
      I_BUKRS        = P_BUKRS
    importing
      E_KOKRS        = GV_KOKRS
    exceptions
      NO_KOKRS_FOUND = 0 "Not mandatory to use CO
      others         = 0.

*   Selection of cost centers "SAPLKMA1->K_KOSTL_SELECTION_RUN
  call function 'K_COSTCENTERS_SELECT' "SAPLKMS1 (langu as param.)
    exporting
      KOKRS           = GV_KOKRS
      DATE_FROM       = GS_FIN_CONTROL-DTINI
      DATE_TO         = GS_FIN_CONTROL-DTFIN
      LANGU           = P_SPRA
      TABNAME         = 'CSKS_EX'
    tables
      IT_RESULT       = LT_CSKS_EX
    exceptions
      NO_RECORD_FOUND = 0 "Not mandatory to use CO
      GROUP_NOT_FOUND = 0
      others          = 0.

  delete LT_CSKS_EX where BUKRS <> P_BUKRS.                 "1325556
  loop at LT_CSKS_EX assigning <LV_CSKS_EX>.
    call function 'K_CSKS_AUTHORITY_CHECK'
      exporting
        ACTVT  = '03'
        KOKRS  = GV_KOKRS
        KOSTL  = <LV_CSKS_EX>-KOSTL
      exceptions
        others = 1.
    if SY-SUBRC <> 0.
      continue. "loop
    endif.
    if LV_COD_CCUS_ANT <> <LV_CSKS_EX>-KOSTL. "doesn't exist yet in the list
      clear LS_I100.
      LS_I100-REG      = 'I100'.
      LS_I100-DT_ALT   = <LV_CSKS_EX>-DATAB. "ersda can't be used
      LS_I100-COD_CCUS = <LV_CSKS_EX>-KOSTL.
      LS_I100-CCUS     = <LV_CSKS_EX>-LTEXT.
      append LS_I100 to GT_I100.
    endif.
    LV_COD_CCUS_ANT = <LV_CSKS_EX>-KOSTL.                   "1339041
  endloop.

  loop at GT_I100 into LS_I100.
    perform FORMAT_RECORD_TO_LINE using LS_I100
                                  changing LS_RESULT.       "1909036
    if GV_APPSV = ABAP_TRUE.
      transfer LS_RESULT to P_APOUT.
    else.
      GS_RESULT = LS_RESULT.
      append GS_RESULT to GT_RESULT.
    endif.
  endloop.
  wait up to 1 seconds.
  write: / 'I100' .
  write: SY-UZEIT.


*  "I150 ALRS

* PERIODIC BALANCES, PERIOD IDENTIFICATION - CARDINALITY N
  data:
    LS_I150     type J_1BECD_I150_3_S,
    LS_SEL_I150 type TP_PERIOD_I150,
    LS_SKC1A    type SKC1A,
    LV_LINES    type SYINDEX.

  clear GT_SEL_I150.
  if GS_FIN_CONTROL-TSL01_IND = ABAP_TRUE.
    LS_SEL_I150-DT_INI = GS_FIN_CONTROL-TSL01_DTI.
    LS_SEL_I150-DT_FIN = GS_FIN_CONTROL-TSL01_DTF.
    LS_SEL_I150-CURRENT_PERIOD = '01'.
    append LS_SEL_I150 to GT_SEL_I150.
  endif.
  if GS_FIN_CONTROL-TSL02_IND = ABAP_TRUE.
    LS_SEL_I150-DT_INI = GS_FIN_CONTROL-TSL02_DTI.
    LS_SEL_I150-DT_FIN = GS_FIN_CONTROL-TSL02_DTF.
    LS_SEL_I150-CURRENT_PERIOD = '02'.
    append LS_SEL_I150 to GT_SEL_I150.
  endif.
  if GS_FIN_CONTROL-TSL03_IND = ABAP_TRUE.
    LS_SEL_I150-DT_INI = GS_FIN_CONTROL-TSL03_DTI.
    LS_SEL_I150-DT_FIN = GS_FIN_CONTROL-TSL03_DTF.
    LS_SEL_I150-CURRENT_PERIOD = '03'.
    append LS_SEL_I150 to GT_SEL_I150.
  endif.
  if GS_FIN_CONTROL-TSL04_IND = ABAP_TRUE.
    LS_SEL_I150-DT_INI = GS_FIN_CONTROL-TSL04_DTI.
    LS_SEL_I150-DT_FIN = GS_FIN_CONTROL-TSL04_DTF.
    LS_SEL_I150-CURRENT_PERIOD = '04'.
    append LS_SEL_I150 to GT_SEL_I150.
  endif.
  if GS_FIN_CONTROL-TSL05_IND = ABAP_TRUE.
    LS_SEL_I150-DT_INI = GS_FIN_CONTROL-TSL05_DTI.
    LS_SEL_I150-DT_FIN = GS_FIN_CONTROL-TSL05_DTF.
    LS_SEL_I150-CURRENT_PERIOD = '05'.
    append LS_SEL_I150 to GT_SEL_I150.
  endif.
  if GS_FIN_CONTROL-TSL06_IND = ABAP_TRUE.
    LS_SEL_I150-DT_INI = GS_FIN_CONTROL-TSL06_DTI.
    LS_SEL_I150-DT_FIN = GS_FIN_CONTROL-TSL06_DTF.
    LS_SEL_I150-CURRENT_PERIOD = '06'.
    append LS_SEL_I150 to GT_SEL_I150.
  endif.
  if GS_FIN_CONTROL-TSL07_IND = ABAP_TRUE.
    LS_SEL_I150-DT_INI = GS_FIN_CONTROL-TSL07_DTI.
    LS_SEL_I150-DT_FIN = GS_FIN_CONTROL-TSL07_DTF.
    LS_SEL_I150-CURRENT_PERIOD = '07'.
    append LS_SEL_I150 to GT_SEL_I150.
  endif.
  if GS_FIN_CONTROL-TSL08_IND = ABAP_TRUE.
    LS_SEL_I150-DT_INI = GS_FIN_CONTROL-TSL08_DTI.
    LS_SEL_I150-DT_FIN = GS_FIN_CONTROL-TSL08_DTF.
    LS_SEL_I150-CURRENT_PERIOD = '08'.
    append LS_SEL_I150 to GT_SEL_I150.
  endif.
  if GS_FIN_CONTROL-TSL09_IND = ABAP_TRUE.
    LS_SEL_I150-DT_INI = GS_FIN_CONTROL-TSL09_DTI.
    LS_SEL_I150-DT_FIN = GS_FIN_CONTROL-TSL09_DTF.
    LS_SEL_I150-CURRENT_PERIOD = '09'.
    append LS_SEL_I150 to GT_SEL_I150.
  endif.
  if GS_FIN_CONTROL-TSL10_IND = ABAP_TRUE.
    LS_SEL_I150-DT_INI = GS_FIN_CONTROL-TSL10_DTI.
    LS_SEL_I150-DT_FIN = GS_FIN_CONTROL-TSL10_DTF.
    LS_SEL_I150-CURRENT_PERIOD = '10'.
    append LS_SEL_I150 to GT_SEL_I150.
  endif.
  if GS_FIN_CONTROL-TSL11_IND = ABAP_TRUE.
    LS_SEL_I150-DT_INI = GS_FIN_CONTROL-TSL11_DTI.
    LS_SEL_I150-DT_FIN = GS_FIN_CONTROL-TSL11_DTF.
    LS_SEL_I150-CURRENT_PERIOD = '11'.
    append LS_SEL_I150 to GT_SEL_I150.
  endif.
  if GS_FIN_CONTROL-TSL12_IND = ABAP_TRUE or
     GS_FIN_CONTROL-TSL13_IND = ABAP_TRUE or              "same as 12
     GS_FIN_CONTROL-TSL14_IND = ABAP_TRUE or              "same as 12
     GS_FIN_CONTROL-TSL15_IND = ABAP_TRUE or              "same as 12
     GS_FIN_CONTROL-TSL16_IND = ABAP_TRUE.                "same as 12
    LS_SEL_I150-DT_INI = GS_FIN_CONTROL-TSL12_DTI.
    LS_SEL_I150-DT_FIN = GS_FIN_CONTROL-TSL12_DTF.
    LS_SEL_I150-CURRENT_PERIOD = '12'. "last month
    append LS_SEL_I150 to GT_SEL_I150.
  endif.
  sort GT_SEL_I150 ascending.
*  SORT IT_SALDO_CONTAS BY RACCT.
  loop at GT_SEL_I150 into LS_SEL_I150.
    clear V_TEM.
    loop at IT_SALDO_CONTAS.
      LV_ACCOUNT_NR = IT_SALDO_CONTAS-RACCT.
      read table T_AGL_A transporting no fields
            with key ACCOUNT_NR = LV_ACCOUNT_NR  binary search.

      if SY-SUBRC ne 0.
        continue.
      endif.
      WL_SALDO_MI2 = 0.
      VMES = LS_SEL_I150-DT_INI+4(2).
      subtract 1 from VMES.

      add IT_SALDO_CONTAS-SLVT to WL_SALDO_MI2.
      do VMES times varying REFE1 from IT_SALDO_CONTAS-SL01 next IT_SALDO_CONTAS-SL02.
        add REFE1 to WL_SALDO_MI2.
      enddo.
      VMES = LS_SEL_I150-DT_INI+4(2).
      clear GS_I155.
      read table T_SLD into W_SLD with key MES   = VMES
                                           HKONT = IT_SALDO_CONTAS-RACCT binary search.
      if SY-SUBRC = 0.
        GS_I155-VL_CRED = W_SLD-VL_CRE.
        GS_I155-VL_DEB = W_SLD-VL_DEB.
      endif.
      if WL_SALDO_MI2 ne 0 or GS_I155-VL_CRED ne 0 or GS_I155-VL_DEB ne 0.
        V_TEM = 'X'.
        exit.
      endif.
      if R_MOEDA = 'X'.
        read table IT_SALDO_CONTAS_2 with key RACCT = IT_SALDO_CONTAS-RACCT binary search.
        WL_SALDO_MI2 = 0.
        VMES = LS_SEL_I150-DT_INI+4(2).
        subtract 1 from VMES.

        add IT_SALDO_CONTAS_2-SLVT to WL_SALDO_MI2.
        do VMES times varying REFE1 from IT_SALDO_CONTAS_2-SL01 next IT_SALDO_CONTAS_2-SL02.
          add REFE1 to WL_SALDO_MI2.
        enddo.
        VMES = LS_SEL_I150-DT_INI+4(2).
        clear GS_I155.
        read table T_SLD into W_SLD with key MES   = VMES
                                             HKONT = IT_SALDO_CONTAS-RACCT binary search.
        if SY-SUBRC = 0.
          GS_I155-VL_CRED = W_SLD-VL_CREF.
          GS_I155-VL_DEB = W_SLD-VL_DEBF.
        endif.
        if WL_SALDO_MI2 ne 0 or GS_I155-VL_CRED ne 0 or GS_I155-VL_DEB ne 0.
          V_TEM = 'X'.
          exit.
        endif.
      endif.

    endloop.

    if V_TEM = 'X'.
      LS_I150-REG    = 'I150'.
      LS_I150-DT_INI = LS_SEL_I150-DT_INI.
      LS_I150-DT_FIN = LS_SEL_I150-DT_FIN.
      perform FORMAT_RECORD_TO_LINE using LS_I150
                                    changing LS_RESULT.     "1909036
      if GV_APPSV = ABAP_TRUE.
        transfer LS_RESULT to P_APOUT.
      else.
        GS_RESULT = LS_RESULT.
        append GS_RESULT to GT_RESULT.
      endif.
      "I155
      loop at IT_SALDO_CONTAS.
        LV_ACCOUNT_NR = IT_SALDO_CONTAS-RACCT.
        read table T_AGL_A transporting no fields
              with key ACCOUNT_NR = LV_ACCOUNT_NR  binary search.

        if SY-SUBRC ne 0.
          continue.
        endif.
        WL_SALDO_MI2 = 0.
        VMES = LS_SEL_I150-DT_INI+4(2).
        subtract 1 from VMES.

        add IT_SALDO_CONTAS-SLVT to WL_SALDO_MI2.
        do VMES times varying REFE1 from IT_SALDO_CONTAS-SL01 next IT_SALDO_CONTAS-SL02.
          add REFE1 to WL_SALDO_MI2.
        enddo.

        clear GS_I155.
        GS_I155-REG       = 'I155'.
        GS_I155-COD_CTA   = IT_SALDO_CONTAS-RACCT.
        GS_I155-COD_CCUS  = ''.

        VMES = LS_SEL_I150-DT_INI+4(2).
        read table T_SLD into W_SLD with key MES   = VMES
                                             HKONT = IT_SALDO_CONTAS-RACCT binary search.
        if SY-SUBRC = 0.
          GS_I155-VL_CRED = W_SLD-VL_CRE.
          GS_I155-VL_DEB = W_SLD-VL_DEB.
        endif.

        WL_SALDO_MI3 = WL_SALDO_MI2.
        if WL_SALDO_MI3 lt 0.
          multiply WL_SALDO_MI3 by -1.
        endif.
        GS_I155-VL_SLD_INI = WL_SALDO_MI3.
        if WL_SALDO_MI2 le 0.
          GS_I155-IND_DC_INI = 'C'.
        else.
          GS_I155-IND_DC_INI = 'D'.
        endif.

        WL_SALDO_MI3 = WL_SALDO_MI2 + GS_I155-VL_DEB - GS_I155-VL_CRED.
        WL_SALDO_MI2 = WL_SALDO_MI3.
        if WL_SALDO_MI3 lt 0.
          multiply WL_SALDO_MI3 by -1.
        endif.

        GS_I155-VL_SLD_FIN = WL_SALDO_MI3.

        if WL_SALDO_MI2 le 0.
          GS_I155-IND_DC_FIN = 'C'.
        else.
          GS_I155-IND_DC_FIN = 'D'.
        endif.


        perform FORMAT_RECORD_TO_LINE using GS_I155
                                    changing LS_RESULT.     "1909036
        clear V_TEM.
        if R_MOEDA = 'X'.
          WL_SALDO_MI2 = 0.
          VMES = LS_SEL_I150-DT_INI+4(2).
          subtract 1 from VMES.
          read table IT_SALDO_CONTAS_2 with key RACCT = IT_SALDO_CONTAS-RACCT binary search.
          if SY-SUBRC = 0.
            add IT_SALDO_CONTAS_2-SLVT to WL_SALDO_MI2.
            do VMES times varying REFE1 from IT_SALDO_CONTAS_2-SL01 next IT_SALDO_CONTAS_2-SL02.
              add REFE1 to WL_SALDO_MI2.
            enddo.
          endif.

          "10
          WL_SALDO_MI3 = WL_SALDO_MI2.
          if WL_SALDO_MI3 lt 0.
            multiply WL_SALDO_MI3 by -1.
          endif.
          if WL_SALDO_MI3 ne 0.
            V_TEM = 'X'.
          endif.
          LV_FIELD_MOE = WL_SALDO_MI3.
          replace '.' with ',' into LV_FIELD_MOE.
          condense LV_FIELD_MOE no-gaps.
          concatenate LS_RESULT LV_FIELD_MOE CONST_SEPARATOR  into LS_RESULT.

          "11
          if WL_SALDO_MI2 le 0.
            GS_I155-IND_DC_INI = 'C'.
          else.
            GS_I155-IND_DC_INI = 'D'.
          endif.
          concatenate LS_RESULT GS_I155-IND_DC_INI CONST_SEPARATOR  into LS_RESULT.

          "12
          clear W_SLD.
          VMES = LS_SEL_I150-DT_INI+4(2).
          read table T_SLD into W_SLD with key MES   = VMES
                                            HKONT = IT_SALDO_CONTAS-RACCT binary search.

          LV_FIELD_MOE = W_SLD-VL_DEBF.
          replace '.' with ',' into LV_FIELD_MOE.
          condense LV_FIELD_MOE no-gaps.
          concatenate LS_RESULT LV_FIELD_MOE CONST_SEPARATOR  into LS_RESULT.
          if W_SLD-VL_DEBF ne 0.
            V_TEM = 'X'.
          endif.

          "13
          LV_FIELD_MOE = W_SLD-VL_CREF.
          replace '.' with ',' into LV_FIELD_MOE.
          condense LV_FIELD_MOE no-gaps.
          concatenate LS_RESULT LV_FIELD_MOE CONST_SEPARATOR  into LS_RESULT.
          if W_SLD-VL_CREF ne 0.
            V_TEM = 'X'.
          endif.

          "14
          WL_SALDO_MI3 = WL_SALDO_MI2 + W_SLD-VL_DEBF - W_SLD-VL_CREF.
          WL_SALDO_MI2 = WL_SALDO_MI3.
          if WL_SALDO_MI3 lt 0.
            multiply WL_SALDO_MI3 by -1.
          endif.
          if  WL_SALDO_MI3 ne 0.
            V_TEM = 'X'.
          endif.
          LV_FIELD_MOE = WL_SALDO_MI3.
          replace '.' with ',' into LV_FIELD_MOE.
          condense LV_FIELD_MOE no-gaps.
          concatenate LS_RESULT LV_FIELD_MOE CONST_SEPARATOR  into LS_RESULT.

          "15
          if WL_SALDO_MI2 le 0.
            GS_I155-IND_DC_INI = 'C'.
          else.
            GS_I155-IND_DC_INI = 'D'.
          endif.
          concatenate LS_RESULT GS_I155-IND_DC_INI CONST_SEPARATOR  into LS_RESULT.
        endif.
        if  GS_I155-VL_SLD_INI ne 0 or GS_I155-VL_CRED ne 0 or GS_I155-VL_DEB ne 0 or GS_I155-VL_SLD_FIN ne 0 or V_TEM = 'X'.
          if GV_APPSV = ABAP_TRUE.
            transfer LS_RESULT to P_APOUT.
          else.
            GS_RESULT = LS_RESULT.
            append GS_RESULT to GT_RESULT.
          endif.
        else.
          subtract 1 from W_TOTAL_J900.
          subtract 1 from W_TOTAL_I990.
          W_CONTADOR-REG_BLC     = 'I155'.
          W_CONTADOR-QTD_REG_BLC = -1.
          collect  W_CONTADOR into T_CONTADOR.
        endif.

      endloop.
    endif.
  endloop.

  free: IT_SALDO_CONTAS,IT_SALDO_CONTAS_2, IT_SALDO_CONTAS_3, IT_CONTAS.
  wait up to 1 seconds.
  write: / 'I150' .
  write: SY-UZEIT.

  data: V_BUDAT type BKPF-BUDAT,
        V_BELNR type BSEG-BELNR.

  clear: V_BUDAT, V_BELNR.
  "I250
  loop at T_BSEG into W_BSEG.
*    IF R_MOEDA = 'X'.
*      IF  W_BSEG-DMBTR = 0 AND W_BSEG-DMBE2 = 0.
*        CONTINUE.
*      ENDIF.
*    ELSE.
*      IF  W_BSEG-DMBTR = 0.
*        CONTINUE.
*      ENDIF.
*    ENDIF.

    if V_BELNR ne W_BSEG-BELNR.
      read table T_DOC into W_DOC with key  BUKRS = W_BSEG-BUKRS
                                            BELNR = W_BSEG-BELNR
                                            GJAHR = W_BSEG-GJAHR binary search.
      GS_I200-REG       = 'I200'.
      GS_I200-NUM_LCTO  = W_DOC-BELNR.
      GS_I200-DT_LCTO   = W_DOC-BUDAT.
      GS_I200-VL_LCTO   = W_DOC-TOTAL.
      GS_I200-IND_LCTO  = W_DOC-TIPO.
*      APPEND GS_I200 TO GT_I200.
      perform FORMAT_RECORD_TO_LINE using GS_I200
                                    changing LS_RESULT.     "1909036
      if R_MOEDA = 'X'.
        LV_FIELD_MOE = W_DOC-TOTALF.
        replace '.' with ',' into LV_FIELD_MOE.
        condense LV_FIELD_MOE no-gaps.
        concatenate LS_RESULT LV_FIELD_MOE CONST_SEPARATOR  into LS_RESULT.
      endif.
      if GV_APPSV = ABAP_TRUE.
        transfer LS_RESULT to P_APOUT.
      else.
        GS_RESULT = LS_RESULT.
        append GS_RESULT to GT_RESULT.
      endif.
    endif.
    V_BUDAT = W_BSEG-BUDAT.
    V_BELNR = W_BSEG-BELNR.

    GS_I250-REG       = 'I250'.
    GS_I250-COD_CTA   = W_BSEG-HKONT.
    GS_I250-COD_CCUS  = ''.
    GS_I250-VL_DC     = W_BSEG-DMBTR.
    if W_BSEG-SHKZG = 'H'.
      GS_I250-IND_DC    = 'C'.
    else.
      GS_I250-IND_DC    = 'D'.
    endif.
    GS_I250-NUM_ARQ   = W_DOC-XBLNR.
    GS_I250-COD_HIST_PAD = ''.
    GS_I250-HIST      = W_BSEG-SGTXT.
    replace all occurrences of CL_ABAP_CHAR_UTILITIES=>NEWLINE
                        in GS_I250-HIST
                        with ''.

    replace all occurrences of CL_ABAP_CHAR_UTILITIES=>CR_LF
                            in GS_I250-HIST
                            with ''.
    GS_I250-COD_PART  = ''.
*    APPEND GS_I250 TO GT_I250.
    if W_BSEG-VBUND is not initial.
      select single SETNAME LINEID SETCLASS
        from SETLEAF
        into (V2_SETNAME, V3_LINEID, V4_SETCLASS)
       where SETNAME eq 'ECD_BLOCO_I250'
         and VALFROM eq W_BSEG-VBUND.

      if SY-SUBRC is initial.
        select single DESCRIPT
         from SETLINET
         into V5_DESCRIPT
        where SETCLASS eq V4_SETCLASS
          and SETNAME  eq V2_SETNAME
          and LINEID   eq V3_LINEID.

        if SY-SUBRC is initial.
          GS_I250-COD_PART = V5_DESCRIPT.
        endif.
      endif.
    endif.

    perform FORMAT_RECORD_TO_LINE using GS_I250
                                  changing LS_RESULT.       "1909036
    if R_MOEDA = 'X'.
      "10 e 11
      LV_FIELD_MOE = W_BSEG-DMBE2.
      replace '.' with ',' into LV_FIELD_MOE.
      condense LV_FIELD_MOE no-gaps.
      concatenate LS_RESULT LV_FIELD_MOE CONST_SEPARATOR  GS_I250-IND_DC CONST_SEPARATOR into LS_RESULT.
    endif.
    if GV_APPSV = ABAP_TRUE.
      transfer LS_RESULT to P_APOUT.
    else.
      GS_RESULT = LS_RESULT.
      append GS_RESULT to GT_RESULT.
    endif.
  endloop.
  free: T_DOC, T_BSEG.
  wait up to 1 seconds.
  write: / 'I200' .
  write: SY-UZEIT.

  if T_ENC[] is not initial.
    clear GT_I350.
    GS_I350-REG    = 'I350'.
    GS_I350-DT_RES = P_DTRES.
    if P_DOCTYP is initial and                              "1493270
      GS_FIN_CONTROL-TOTAL_ACC_IND_J = ABAP_TRUE.           "1493270
      GS_I350-DT_RES = GS_FIN_CONTROL-DTFIN.                "1493270
    endif.                                                  "1493270
*  APPEND GS_I350 TO GT_I350.
    perform FORMAT_RECORD_TO_LINE using GS_I350
                                     changing LS_RESULT.    "1909036
    if GV_APPSV = ABAP_TRUE.
      transfer LS_RESULT to P_APOUT.
    else.
      GS_RESULT = LS_RESULT.
      append GS_RESULT to GT_RESULT.
    endif.
  endif.

  loop at T_ENC into W_ENC.
    read table  GT_FSV into LS_FSV
            with key ACCOUNT_NR = W_ENC-HKONT
                     IS_ANALY_SINT = 'A'.
    if SY-SUBRC = 0.
      if LS_FSV-SPED_INT_CODE = CONST_IC_ACC_INCOME         "2023757
         or LS_FSV-SPED_INT_CODE    = CONST_IC_ACC_LOSSES.
        GS_I355-REG       = 'I355'.
        GS_I355-COD_CTA   = W_ENC-HKONT.
        GS_I355-COD_CCUS  = ''.
        if W_ENC-TOTAL le 0.
          multiply W_ENC-TOTAL by -1.
          GS_I355-IND_DC = 'D'.
        else.
          GS_I355-IND_DC = 'C'.
        endif.
        GS_I355-VL_CTA    = W_ENC-TOTAL.
*        APPEND GS_I355 TO GT_I355.
        perform FORMAT_RECORD_TO_LINE using GS_I355
                                        changing LS_RESULT. "1909036
        if R_MOEDA = 'X'.
          if W_ENC-TOTALF le 0.
            multiply W_ENC-TOTALF by -1.
            GS_I355-IND_DC = 'D'.
          else.
            GS_I355-IND_DC = 'C'.
          endif.
          LV_FIELD_MOE = W_ENC-TOTALF.
          replace '.' with ',' into LV_FIELD_MOE.
          condense LV_FIELD_MOE no-gaps.
          concatenate LS_RESULT LV_FIELD_MOE CONST_SEPARATOR GS_I355-IND_DC CONST_SEPARATOR  into LS_RESULT.
        endif.
        if GV_APPSV = ABAP_TRUE.
          transfer LS_RESULT to P_APOUT.
        else.
          GS_RESULT = LS_RESULT.
          append GS_RESULT to GT_RESULT.
        endif.
      endif.
    endif.
  endloop.

  wait up to 1 seconds.
  write: / 'I350' .
  write: SY-UZEIT.


*I990
  add 1 to W_TOTAL_I990.
  GS_I990-REG        = 'I990'.
  GS_I990-QTD_LIN_I  = W_TOTAL_I990.
  perform FORMAT_RECORD_TO_LINE using GS_I990
                                    changing LS_RESULT.     "1909036
  if GV_APPSV = ABAP_TRUE.
    transfer LS_RESULT to P_APOUT.
  else.
    GS_RESULT = LS_RESULT.
    append GS_RESULT to GT_RESULT.
  endif.

  "J001
  clear GT_J001.
  GS_J001-REG     = 'J001'.
  GS_J001-IND_DAD = CONST_BLOCK_WITH.
*  APPEND GS_J001 TO GT_J001.
  perform FORMAT_RECORD_TO_LINE using GS_J001
                                   changing LS_RESULT.      "1909036
  if GV_APPSV = ABAP_TRUE.
    transfer LS_RESULT to P_APOUT.
  else.
    GS_RESULT = LS_RESULT.
    append GS_RESULT to GT_RESULT.
  endif.

  data:
    LS_NEXT_J150     type J_1BECD_J150_3_S,
    LV_CURRENT_TABIX type SYTABIX,
    LV_NEXT_TABIX    type SYTABIX,
    LV_MAX_TABIX     type SYTABIX.


  "J005
  clear GT_J005.
*loop <--- check situation where j005 has more than 1 record
  clear GS_J005.
  GS_J005-REG      = 'J005'.
  GS_J005-DT_INI   = GS_FIN_CONTROL-DTINI. "p_dtini.
  GS_J005-DT_FIN   = GS_FIN_CONTROL-DTFIN. "p_dtfin.
  GS_J005-ID_DEM   = P_IDDEM.
  GS_J005-CAB_DEM  = P_CABDEM.
*  APPEND GS_J005 TO GT_J005.
  perform FORMAT_RECORD_TO_LINE using GS_J005
                                   changing LS_RESULT.      "1909036
  if GV_APPSV = ABAP_TRUE.
    transfer LS_RESULT to P_APOUT.
  else.
    GS_RESULT = LS_RESULT.
    append GS_RESULT to GT_RESULT.
  endif.

  "J100
  loop at T_AGL into W_AGL.
    if W_AGL-IS_ANALY_SINT = 'A'.
      continue.
    endif.
    if ( W_AGL-SPED_INT_CODE ne CONST_IC_ACC_INCOME and W_AGL-SPED_INT_CODE  ne CONST_IC_ACC_LOSSES ).
      LS_J100-REG             = 'J100'.
      LS_J100-COD_AGL         = W_AGL-ACCOUNT_NR.
      LS_J100-NIVEL_AGL       = W_AGL-LEVEL.
      case W_AGL-SPED_INT_CODE.
        when CONST_IC_ACC_ASSET.
          LS_J100-IND_GRP_BAL =  CONST_ASSET.
        when CONST_IC_ACC_LIABILITY
          or CONST_IC_ACC_NET_EQUITY.
          LS_J100-IND_GRP_BAL =  CONST_LIAB_AND_NETEQ.
      endcase.
      LS_J100-DESCR_COD_AGL   = W_AGL-DESCRIPTION.
      LS_J100-VL_CTA_INI      = W_AGL-TOTAL_INI.
      LS_J100-VL_CTA          = W_AGL-TOTAL_FIN.
      "
      if LS_J100-VL_CTA_INI > 0. "D Debtor                        "1909036
        LS_J100-IND_DC_BAL_INI = CONST_SPED_DEBIT.          "1909036
      elseif LS_J100-VL_CTA_INI <= 0. "C Creditor                 "1909036
        "Zero is considered Credit                                "1909036
        LS_J100-IND_DC_BAL_INI = CONST_SPED_CREDIT.         "1909036
        "Always absolute value                                    "1909036
        LS_J100-VL_CTA_INI = - LS_J100-VL_CTA_INI.          "1909036
      endif.                                                "1909036

      if LS_J100-VL_CTA > 0.
        LS_J100-IND_DC_BAL = CONST_SPED_DEBIT.  "D Debtor
      elseif LS_J100-VL_CTA <= 0.                          "#EC BOOL_OK
        LS_J100-IND_DC_BAL = CONST_SPED_CREDIT. "C Creditor
        LS_J100-VL_CTA     = - LS_J100-VL_CTA.  "Always absolute value
      endif.
      if LS_J100-VL_CTA_INI ne 0 or LS_J100-VL_CTA ne 0.
*        APPEND LS_J100 TO GT_J100.
        LS_RESULT = 'J100'.
        perform FORMAT_RECORD_TO_LINE using LS_J100
                                         changing LS_RESULT.
        if GV_APPSV = ABAP_TRUE.
          transfer LS_RESULT to P_APOUT.
        else.
          GS_RESULT = LS_RESULT.
          append GS_RESULT to GT_RESULT.
        endif.
      endif.
    else.
      clear LS_J150.
      multiply W_AGL-TOTAL       by -1.
      multiply W_AGL-TOTAL_PER_A by -1.
      LS_J150-COD_AGL        = W_AGL-ACCOUNT_NR.
      LS_J150-NIVEL_AGL      = W_AGL-LEVEL.
      LS_J150-DESCR_COD_AGL  = W_AGL-DESCRIPTION.
      LS_J150-VL_CTA         = W_AGL-TOTAL.
      LS_J150-VL_CTA_ULT_DRE = W_AGL-TOTAL_PER_A.
      LS_J150-IND_VL_ULT_DRE = ''.
      append LS_J150 to GT_J150.
    endif.
  endloop.

  free T_AGL .
  wait up to 1 seconds.
  write: / 'J100' .
  write: SY-UZEIT.

  if T_ENC[] is not initial. "Se tiver encerramento
    describe table GT_J150 lines LV_MAX_TABIX.
    loop at GT_J150 into LS_J150.
      LV_CURRENT_TABIX = SY-TABIX.
      LV_NEXT_TABIX = LV_CURRENT_TABIX + 1.
      clear LS_NEXT_J150.
      if LV_NEXT_TABIX <= LV_MAX_TABIX.
        read table GT_J150 into LS_NEXT_J150 index LV_NEXT_TABIX.
      endif.
      if LS_J150-VL_CTA >= 0. "Zero is considered Revenue
        if LS_NEXT_J150-NIVEL_AGL > LS_J150-NIVEL_AGL. "line with totals
          LS_J150-IND_VL = CONST_POSITIVE_SUBT.
        else. "lower nodes at the hierarchy
          LS_J150-IND_VL = CONST_REVENUE.
        endif.
      elseif LS_J150-VL_CTA < 0.
        if LS_NEXT_J150-NIVEL_AGL > LS_J150-NIVEL_AGL. "line with totals
          LS_J150-IND_VL = CONST_NEGATIVE_SUBT.
        else. "lower nodes at the hierarchy
          LS_J150-IND_VL = CONST_EXPENSE.
        endif.
        LS_J150-VL_CTA = - LS_J150-VL_CTA.  "Always absolute value
      endif.
      "exercicio anterior ALRS
      if LS_J150-VL_CTA_ULT_DRE >= 0. "Zero is considered Revenue
        if LS_NEXT_J150-NIVEL_AGL > LS_J150-NIVEL_AGL. "line with totals
          LS_J150-IND_VL_ULT_DRE = CONST_POSITIVE_SUBT.
        else. "lower nodes at the hierarchy
          LS_J150-IND_VL_ULT_DRE = CONST_REVENUE.
        endif.
      elseif LS_J150-VL_CTA_ULT_DRE < 0.
        if LS_NEXT_J150-NIVEL_AGL > LS_J150-NIVEL_AGL. "line with totals
          LS_J150-IND_VL_ULT_DRE = CONST_NEGATIVE_SUBT.
        else. "lower nodes at the hierarchy
          LS_J150-IND_VL_ULT_DRE = CONST_EXPENSE.
        endif.
        LS_J150-VL_CTA_ULT_DRE = - LS_J150-VL_CTA_ULT_DRE.  "Always absolute value
      endif.

      LS_J150-REG = 'J150'.
      modify GT_J150 from LS_J150 index LV_CURRENT_TABIX.
      LS_RESULT = 'J150'.
      perform FORMAT_RECORD_TO_LINE using LS_J150
                                         changing LS_RESULT. "1909036
      if GV_APPSV = ABAP_TRUE.
        transfer LS_RESULT to P_APOUT.
      else.
        GS_RESULT = LS_RESULT.
        append GS_RESULT to GT_RESULT.
      endif.

    endloop.
    free GT_J150.
    wait up to 1 seconds.
    write: / 'J150' .
    write: SY-UZEIT.
  endif.

  free T_ENC.



  "J800 NOVO (ALRS
  read table GT_J150 into LS_J150 index 1.
  if SY-SUBRC = 0.
    perform PROCESS_RAW_REGISTER using 'J' LS_J150 CONST_REG_J800.
    perform PROCESS_RAW_REGISTER using 'J' LS_J150 CONST_REG_J801.
  endif.


  "J900
* Select Persons Responsible Data
*  SELECT * FROM J_1BECD_CUST03 INTO TABLE GT_ACCOUNTANT
*    WHERE BUKRS = P_BUKRS.

  " CRIADA ESSA ESTRUTURA PORQUE O CAMPO IDENT_CPF TEM QUE TER 14 E NO STANDARD TEM 11 (AGUARDAR NOTA SAP PARA VOLTAR AO NORMAL) 02/05/2017
  types: begin of TY_930,
           REG               type  J_1BECD_REGISTER,
           IDENT_NOM(255),
           IDENT_CPF         type  ZSPED005-IDENT_CPF,
           IDENT_QUALIF(255),
           COD_ASSIN         type  J_1BECD_COD_ASSIN,
           IND_CRC           type  J_1BECD_IND_CRC,
           EMAIL             type  J_1BECD_ACCOUNT_EMAIL,
           FONE              type  J_1BECD_ACCOUNT_PHONE,
           UF_CRC            type  J_1BECD_UF_CRC,
           NUM_SEQ_CRC       type  J_1BECD_NUM_SEQ_CRC,
           DT_CRC            type  J_1BECD_DT_CRC,
           IND_RESP_LEGAL    type  J_1BECD_IND_RESP_LEGAL,
         end of TY_930.

  data: GS_ACCOUNTANTZ type ZSPED005,
        GT_ACCOUNTANTZ type table of ZSPED005,
        GS_J930Z       type TY_930.

  if P_ESC = 0.
    select *
      from ZSPED005 into table GT_ACCOUNTANTZ
      where BUKRS     eq P_BUKRS
      and   COD_ASSIN not in ( '910', '920' ).
  else.
    select *
      from ZSPED005 into table GT_ACCOUNTANTZ
  where BUKRS eq P_BUKRS.
  endif.


  clear GT_J900.
  GS_J900-DNRC_ENCER = text-901.
  GS_J900-NUM_ORD    = P_NUMORD.
  GS_J900-NAT_LIVR   = P_NATLIV.
  if not GV_NOME is initial.
    GS_J900-NOME    = GV_NOME.
  else.
    GS_J900-NOME    = GS_BRANCH_DATA-NAME.
  endif.

  add 12 to W_TOTAL_J900. "propria linha + Linhas apos bloco
  describe table GT_ACCOUNTANTZ lines GV_FSV_CNT.
  add GV_FSV_CNT to W_TOTAL_J900.
  describe table T_CONTADOR lines GV_FSV_CNT.
  add GV_FSV_CNT to W_TOTAL_J900.
  "alrs total

  GS_J900-REG            = 'J900'.
  GS_J900-QTD_LIN = W_TOTAL_J900.
  W_TOTAL = W_TOTAL_J900.

  call function 'CONVERSION_EXIT_ALPHA_INPUT'
    exporting
      INPUT  = GS_J900-QTD_LIN
    importing
      OUTPUT = GS_J900-QTD_LIN.

  GS_J900-DT_INI_ESCR = GS_FIN_CONTROL-DTINI. "p_dtini.
  GS_J900-DT_FIN_ESCR = GS_FIN_CONTROL-DTFIN. "p_dtfin.
  append GS_J900 to GT_J900.
  perform FORMAT_RECORD_TO_LINE using GS_J900
                                   changing LS_RESULT.      "1909036
  if GV_APPSV = ABAP_TRUE.
    transfer LS_RESULT to P_APOUT.
  else.
    GS_RESULT = LS_RESULT.
    append GS_RESULT to GT_RESULT.
  endif.

  "J930
  data: LV_QUALIF type J_1BECD_CUST02T-IDENT_QUALIF.
  clear GT_J930.

  loop at GT_ACCOUNTANTZ into GS_ACCOUNTANTZ.
    clear GS_J930.
    GS_J930-REG           = 'J930'.
    GS_J930-IDENT_NOM     = GS_ACCOUNTANTZ-IDENT_NOM.
    GS_J930-IDENT_CPF     = GS_ACCOUNTANTZ-IDENT_CPF.
    GS_J930-EMAIL         = GS_ACCOUNTANTZ-EMAIL.           "1909036
    GS_J930-FONE          = GS_ACCOUNTANTZ-FONE.            "1909036
    GS_J930-UF_CRC        = GS_ACCOUNTANTZ-UF_CRC.          "1909036
    GS_J930-NUM_SEQ_CRC   = GS_ACCOUNTANTZ-NUM_SEQ_CRC.     "1909036
    GS_J930-DT_CRC        = GS_ACCOUNTANTZ-DT_CRC.          "1909036
    GS_J930-IND_RESP_LEGAL = GS_ACCOUNTANTZ-IND_RESP_LEGAL.
    if GS_J930-IDENT_CPF is not initial.
      translate GS_J930-IDENT_CPF
          using '- + * / \ . : ; , _ ( ) [ ] # < > '.
      condense GS_J930-IDENT_CPF no-gaps.
    endif.

    GS_J930-COD_ASSIN     = GS_ACCOUNTANTZ-COD_ASSIN.

    select IDENT_QUALIF from J_1BECD_CUST02T into LV_QUALIF
    where SPRAS = P_SPRA and
          COD_ASSIN = GS_ACCOUNTANTZ-COD_ASSIN.
      GS_J930-IDENT_QUALIF  = LV_QUALIF.
    endselect.

    GS_J930-IND_CRC       = GS_ACCOUNTANTZ-IND_CRC.

    if GREF_BADI_J_1BECD is bound.                          "1909036
      if P_BADI = CONST_BADI.                               "1909036
        call badi GREF_BADI_J_1BECD->FILL_REGISTER_J930     "1909036
          changing                                          "1909036
            CS_REG_J930 = GS_J930.                          "1909036
      endif.                                                "1909036
    endif.                                                  "1909036

    append GS_J930 to GT_J930.
    "
    move-corresponding GS_J930 to GS_J930Z.
    GS_J930Z-IDENT_CPF     = GS_ACCOUNTANTZ-IDENT_CPF.
    perform FORMAT_RECORD_TO_LINE using GS_J930Z
                                   changing LS_RESULT.      "1909036
    if GV_APPSV = ABAP_TRUE.
      transfer LS_RESULT to P_APOUT.
    else.
      GS_RESULT = LS_RESULT.
      append GS_RESULT to GT_RESULT.
    endif.
  endloop.

  "J935
  if P_INDEGP = '1'. "Indicador Empresa Grande Porte
    data: GS_ZSPED006 type ZSPED006.

    select single * from ZSPED006 into GS_ZSPED006
     where BUKRS  eq P_BUKRS.

    if SY-SUBRC = 0.
      clear GT_J935.
      GS_J935-REG     = 'J935'.
      GS_J935-NOME_AUDITOR    = GS_ZSPED006-NOME_AUDITOR.
      GS_J935-COD_CVM_AUDITOR = GS_ZSPED006-COD_CVM_AUDITOR.

      perform FORMAT_RECORD_TO_LINE using GS_J935
                                 changing LS_RESULT.        "1909036
      if GV_APPSV = ABAP_TRUE.
        transfer LS_RESULT to P_APOUT.
      else.
        GS_RESULT = LS_RESULT.
        append GS_RESULT to GT_RESULT.
      endif.
    endif.
  endif.

  "J990
  add 1 to W_TOTAL_J990.
  GS_J990-REG            = 'J990'.
  GS_J990-QTD_LIN_J      = W_TOTAL_J990.
  append GS_J900 to GT_J900.
  perform FORMAT_RECORD_TO_LINE using GS_J990
                                   changing LS_RESULT.      "1909036
  if GV_APPSV = ABAP_TRUE.
    transfer LS_RESULT to P_APOUT.
  else.
    GS_RESULT = LS_RESULT.
    append GS_RESULT to GT_RESULT.
  endif.

  if P_CONS = 'S'. "Ind. Escrituração Consolidada
    if P_LAYOUT >= 6.                                       "2416906
      "K001
      clear GT_K001.
      GS_K001-REG     = 'K001'.
      GS_K001-IND_DAD = CONST_BLOCK_WITH.

      perform FORMAT_RECORD_TO_LINE using GS_K001
                                       changing LS_RESULT.  "1909036
      if GV_APPSV = ABAP_TRUE.
        transfer LS_RESULT to P_APOUT.
      else.
        GS_RESULT = LS_RESULT.
        append GS_RESULT to GT_RESULT.
      endif.
      "K990
      clear GT_K990.
      GS_K990-REG       = 'K990'.
      GS_K990-QTD_LIN_K = 2.

      perform FORMAT_RECORD_TO_LINE using GS_K990
                                       changing LS_RESULT.  "1909036
      if GV_APPSV = ABAP_TRUE.
        transfer LS_RESULT to P_APOUT.
      else.
        GS_RESULT = LS_RESULT.
        append GS_RESULT to GT_RESULT.
      endif.

    endif.
  endif.

  "9001
  GS_9001-REG      = '9001'.
  GS_9001-IND_DAD = CONST_BLOCK_WITH.
  append GS_9001 to GT_9001.
  perform FORMAT_RECORD_TO_LINE using GS_9001
                                   changing LS_RESULT.      "1909036
  if GV_APPSV = ABAP_TRUE.
    transfer LS_RESULT to P_APOUT.
  else.
    GS_RESULT = LS_RESULT.
    append GS_RESULT to GT_RESULT.
  endif.

  W_CONTADOR-REG_BLC     = '9990'.
  W_CONTADOR-QTD_REG_BLC = 1.
  collect  W_CONTADOR into T_CONTADOR.

  W_CONTADOR-REG_BLC     = '9999'.
  W_CONTADOR-QTD_REG_BLC = 1.
  collect  W_CONTADOR into T_CONTADOR.

  W_CONTADOR-REG_BLC     = '9900'.
  W_CONTADOR-QTD_REG_BLC = 1.
  collect  W_CONTADOR into T_CONTADOR.

  loop at T_CONTADOR into W_CONTADOR.
    move-corresponding W_CONTADOR to GS_9900.
    GS_9900-REG = '9900'.
    append GS_9900 to GT_9900.
    perform FORMAT_RECORD_TO_LINE using GS_9900
                                 changing LS_RESULT.        "1909036
    if GV_APPSV = ABAP_TRUE.
      transfer LS_RESULT to P_APOUT.
    else.
      GS_RESULT = LS_RESULT.
      append GS_RESULT to GT_RESULT.
    endif.
  endloop.

  "9990
  data: LV_COUNT_9001 type I,                               "2013607
        LV_COUNT_9900 type I.                               "2013607

  clear: LV_COUNT_9001,                                     "2013607
         LV_COUNT_9900,                                     "2013607
         GT_9990,                                           "2013607
         GS_9990.                                           "2013607

  describe table GT_9001 lines LV_COUNT_9001.               "2013607
  describe table GT_9900 lines LV_COUNT_9900.               "2013607

  GS_9990-REG     = '9990'.
  add LV_COUNT_9001 to GS_9990-QTD_LIN_9.                   "2013607
  add LV_COUNT_9900 to GS_9990-QTD_LIN_9.                   "2013607
  add 1             to GS_9990-QTD_LIN_9. " Reg 9990      "2013607
  add 1             to GS_9990-QTD_LIN_9. " Reg 9999      "2013607

  append GS_9990 to GT_9990.
  perform FORMAT_RECORD_TO_LINE using GS_9990
                                changing LS_RESULT.         "1909036
  if GV_APPSV = ABAP_TRUE.
    transfer LS_RESULT to P_APOUT.
  else.
    GS_RESULT = LS_RESULT.
    append GS_RESULT to GT_RESULT.
  endif.

  clear GS_9999.
  GS_9999-REG = '9999'.
  GS_9999-QTD_LIN = W_TOTAL.
*  ADD W_TOTAL_0990 TO GS_9999-QTD_LIN.                      "2013607
*  ADD W_TOTAL_9990 TO GS_9999-QTD_LIN.                      "2013607
*  ADD W_TOTAL_I990 TO GS_9999-QTD_LIN.                      "2013607
*  ADD W_TOTAL_J990 TO GS_9999-QTD_LIN.                      "2013607
*  ADD 1            TO GS_9999-QTD_LIN.                      "2013607

  append GS_9999 to GT_9999.

  perform FORMAT_RECORD_TO_LINE using GS_9999
                                changing LS_RESULT.         "1909036
  if GV_APPSV = ABAP_TRUE.
    transfer LS_RESULT to P_APOUT.
  else.
    GS_RESULT = LS_RESULT.
    append GS_RESULT to GT_RESULT.
  endif.
  wait up to 1 seconds.
  write: / '9999' .
  write: SY-UZEIT.


endform.                    " F_BLOCO_I

*&      Form  format_record_to_line
*&---------------------------------------------------------------------*
form FORMAT_RECORD_TO_LINE using IS_RECORD type ANY
                        changing CS_RESULT type TP_RESULT_LINE.

  data:
*   auxiliar string to keep a formatted field
    LV_FIELD type TP_RESULT_LINE.

  data: LV_TYPE(1) type C.

  data  XCAMPO(30).
  data  XREG(4).

  clear XCAMPO.
  data: LS_VERSIONS       type        TP_VERSIONS,          "1909036
        LS_RECORD_VERSION type ref to DATA.                 "1909036

  field-symbols: <LS_RECORD_VERSION> type ANY,              "1909036
                 <COMPONENT_VERSION> type ANY.              "1909036

  field-symbols: <COMPONENT> type ANY.
  if  CS_RESULT is not initial.
    if CS_RESULT(4) = 'J100'.
      XREG = 'J100'.
      perform GET_LAYOUT_TYPE using 'J100'
                                  P_LAYOUT                  "1909036
                         changing LS_VERSIONS.              "190903
      XCAMPO = 'X'.
    elseif CS_RESULT(4) = 'J150'.
      XREG = 'J150'.
      perform GET_LAYOUT_TYPE using 'J150'
                                  P_LAYOUT                  "1909036
                         changing LS_VERSIONS.              "190903
      XCAMPO = 'X'.
    endif.
  endif.
  if XCAMPO is initial.
    XREG = IS_RECORD(4).
    perform GET_LAYOUT_TYPE using IS_RECORD(4)              "1909036
                                  P_LAYOUT                  "1909036
                         changing LS_VERSIONS.              "190903
  endif.


  clear CS_RESULT.

  if LS_VERSIONS-TYPE is initial.                           "1909036
    exit.                                                   "1909036
  endif.                                                    "1909036
  do.
    assign component SY-INDEX of structure IS_RECORD to <COMPONENT>.
    if SY-SUBRC ne 0.
      exit. "break loop when no more structure components exist
    endif.

    "verify version of layout and compare the component     "1909036
    create data LS_RECORD_VERSION type (LS_VERSIONS-TYPE).  "1909036
    assign LS_RECORD_VERSION->* to <LS_RECORD_VERSION>.     "1909036
    assign component SY-INDEX of structure                  "1909036
      <LS_RECORD_VERSION> to <COMPONENT_VERSION>.           "1909036
    if SY-SUBRC ne 0.                                       "1909036
      exit.                                                 "1909036
    endif.

*------------------FORMAT_FIELD--------------------*
    describe field <COMPONENT> type LV_TYPE.
* --- By default, output field is equal to input field (in char)
    LV_FIELD = <COMPONENT>.          "copy field to char type

* --- Processing of special formatting options, depending of type
    case LV_TYPE.
      when 'C' or 'g'. "g = String
* --- No separator is allowed inside strings
        replace all occurrences of CONST_SEPARATOR
                                in LV_FIELD
                                with CONST_NOTHING.
      when 'P'.
* --- Change decimal separator and delete spaces
        replace '.' with ',' into LV_FIELD.
        condense LV_FIELD no-gaps.
      when 'D'.
* --- Rearrange position of DD, MM amd YYYY
        if LV_FIELD = '00000000'.
          LV_FIELD = SPACE. "SPED format for empty DATE field
        else.
          concatenate LV_FIELD+6(2) LV_FIELD+4(2) LV_FIELD+0(4)
             into LV_FIELD.
        endif.
      when 'N'.
* --- Delete leading zeros
        shift LV_FIELD left deleting leading '0'.
        if LV_FIELD is initial.
          LV_FIELD = '0'. "always keep one zero
        endif.
        if XREG eq '0000' and SY-INDEX eq 19 and LV_FIELD = '0'.
          clear LV_FIELD.
        endif.

    endcase.
*------------------FORMAT_FIELD--------------------*

* --- each new field comes after a separator (even if empty)
    concatenate CS_RESULT CONST_SEPARATOR LV_FIELD
           into CS_RESULT.
  enddo.

  if CS_RESULT+1(1) = '0'.
    add 1 to W_TOTAL_0990.
  elseif CS_RESULT+1(1) = 'I'.
    add 1 to W_TOTAL_I990.
  elseif CS_RESULT+1(1) = 'J'.
    add 1 to W_TOTAL_J990.
  elseif CS_RESULT+1(4) = '9900'.
    add 1 to W_TOTAL_9990.
  endif.

  W_CONTADOR-REG_BLC     = CS_RESULT+1(4).
  W_CONTADOR-QTD_REG_BLC = 1.
  collect  W_CONTADOR into T_CONTADOR.

* --- add final separator
  concatenate CS_RESULT CONST_SEPARATOR into CS_RESULT.
  add 1 to W_TOTAL_J900.

endform.                    "FORMAT_RECORD_TO_LINE

*&---------------------------------------------------------------------*
*&      Form  get_layout_type
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->IV_REGISTER    text
*      -->P_LAYOUT       text
*      -->C_LAYOUT_TYPE  text
*----------------------------------------------------------------------*
form GET_LAYOUT_TYPE using IV_REGISTER type J_1BECD_REGISTER
                           P_LAYOUT type J_1BECD_COD_VER
                     changing C_LAYOUT_TYPE.

  read table GT_VERSIONS
    with key REGISTER = IV_REGISTER
             LAYOUT = P_LAYOUT
    into C_LAYOUT_TYPE.

endform.                    "get_layout_type
*&---------------------------------------------------------------------*
*&  Include           J_1BECD_MAIN_OUT                                 *
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  f4help_local_fname
*&---------------------------------------------------------------------*
form F4HELP_LOCAL_FNAME changing CV_FILENAME type LOCALFILE.

  data: LV_PATH       type DXFIELDS-LONGPATH,
        LV_ABEND_FLAG type DXFIELDS-ABENDFLAG.
  call function 'F4_DXFILENAME_TOPRECURSION'
    exporting
      I_LOCATION_FLAG = 'P'
*     I_SERVER        = '?'
*     I_PATH          =
*     FILEMASK        = '*.*'
*     FILEOPERATION   = 'R'
    importing
      O_PATH          = LV_PATH
      ABEND_FLAG      = LV_ABEND_FLAG
    exceptions
      RFC_ERROR       = 1
      ERROR_WITH_GUI  = 2
      others          = 3.
  if SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  elseif LV_ABEND_FLAG is initial.
    CV_FILENAME = LV_PATH.
  endif.

endform.                    "f4help_local_fname
*&---------------------------------------------------------------------*
*&      Form  f4help_applserver_fname
*&---------------------------------------------------------------------*
form F4HELP_APPLSERVER_FNAME changing CV_FILENAME type LOCALFILE.

  data: LV_PATH       type DXFIELDS-LONGPATH,
        LV_ABEND_FLAG type DXFIELDS-ABENDFLAG.
  call function 'F4_DXFILENAME_TOPRECURSION'
    exporting
      I_LOCATION_FLAG = 'A'
*     I_SERVER        = '?'
*     I_PATH          =
*     FILEMASK        = '*.*'
*     FILEOPERATION   = 'R'
    importing
      O_PATH          = LV_PATH
      ABEND_FLAG      = LV_ABEND_FLAG
    exceptions
      RFC_ERROR       = 1
      ERROR_WITH_GUI  = 2
      others          = 3.
  if SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  elseif LV_ABEND_FLAG is initial.
    CV_FILENAME = LV_PATH.
  endif.

endform.                    "f4help_applserver_fname
**&---------------------------------------------------------------------*
**&      Form  file_upload
**&---------------------------------------------------------------------*
*FORM FILE_UPLOAD  USING UV_XPCIN  TYPE XFELD
*                        UV_PCIN   TYPE J_1BECD_ATTACHMENT
*                        UV_XAPIN  TYPE XFELD
*                        UV_APIN   TYPE J_1BECD_ATTACHMENT
*                  CHANGING IV_FILENAME TYPE STRING.
*
*  DATA LV_FILENAME_AS   TYPE LOCALFILE.
*  DATA LS_FILE          TYPE TP_ROW_FILE.
*  DATA LV_ISFILEOPEN    TYPE ABAP_BOOL.
*  DATA LV_CHECK_FILE    TYPE ABAP_BOOL.
*  CONSTANTS LOGICAL_FILENAME TYPE FILENAME-FILEINTERN
*              VALUE 'FI_SPED_ECD_INPUT_FILE'.
*
*  REFRESH IT_FILE.
*
** ------- Upload from presentation server ---------------------
*  IF UV_XPCIN = 'X'.
*    IF IV_FILENAME IS INITIAL.
*      IV_FILENAME = UV_PCIN. "use filename indicated in selection screen
*    ELSE.
*      LV_CHECK_FILE = ABAP_TRUE.
*    ENDIF.
*    CALL FUNCTION 'GUI_VSS_UPLOAD'
*      EXPORTING
*        FILENAME        = IV_FILENAME
*      TABLES
*        DATA_TAB        = IT_FILE
*      EXCEPTIONS
*        FILE_OPEN_ERROR = 1
*        FILE_READ_ERROR = 2
*        BAD_DATA_FORMAT = 2
*        NO_AUTHORITY    = 3
*        ACCESS_DENIED   = 3
*        OTHERS          = 4.
*    IF SY-SUBRC <> 0 AND LV_CHECK_FILE = ABAP_TRUE.
*      SY-MSGTY = 'E'.
*      SY-MSGID = 'J_1BECD'.
*      IF SY-SUBRC = 1.
*        SY-MSGNO = '022'.
*      ELSEIF SY-SUBRC = 2.
*        SY-MSGNO = '023'.
*      ELSEIF SY-SUBRC = 3.
*        SY-MSGNO = '024'.
*      ENDIF.
*      SY-MSGV1 = IV_FILENAME.
*      CALL FUNCTION 'J_1BECD_LOG_ADD_MESSAGE'
*        EXPORTING
*          IV_EXCNO = GV_EXCNO.
*    ENDIF.
*
** ------- Upload from application server ---------------------
*  ELSEIF UV_XAPIN = 'X'.
*    IF IV_FILENAME IS INITIAL.
*      IV_FILENAME = UV_APIN. "use filename indicated in selection screen
*    ELSE.
*      LV_CHECK_FILE = ABAP_TRUE.
*    ENDIF.
*    LV_FILENAME_AS = IV_FILENAME.
*    TRY.
*        CALL FUNCTION 'FILE_VALIDATE_NAME'
*          EXPORTING
*            LOGICAL_FILENAME  = LOGICAL_FILENAME
*          CHANGING
*            PHYSICAL_FILENAME = LV_FILENAME_AS
*          EXCEPTIONS
*            OTHERS            = 1.
*
*        IF SY-SUBRC <> 0.
*          MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*        ELSE.
*          OPEN DATASET LV_FILENAME_AS FOR INPUT
*                                      IN TEXT MODE
*                                      ENCODING DEFAULT
*                                      WITH WINDOWS LINEFEED.
*          IF SY-SUBRC <> 0.
*            IF LV_CHECK_FILE = ABAP_TRUE.
*              SY-MSGTY = 'E'.
*              SY-MSGID = 'J_1BECD'.
*              SY-MSGNO = '022'.
*              SY-MSGV1 = IV_FILENAME.
*              CALL FUNCTION 'J_1BECD_LOG_ADD_MESSAGE'
*                EXPORTING
*                  IV_EXCNO = GV_EXCNO.
*            ENDIF.
*          ELSE.
*            LV_ISFILEOPEN = ABAP_TRUE.
*            DO.
*              CLEAR LS_FILE.
*              READ DATASET LV_FILENAME_AS INTO LS_FILE-CONTENT.
*              IF SY-SUBRC NE 0.
*                EXIT.
*              ENDIF.
*              APPEND LS_FILE TO IT_FILE.
*            ENDDO.
*            CLOSE DATASET LV_FILENAME_AS.
*          ENDIF.
*        ENDIF.
*      CATCH CX_SY_FILE_IO.
** Filename refers to directory name
*        IF LV_ISFILEOPEN = ABAP_TRUE.
*          CLOSE DATASET LV_FILENAME_AS.
*        ENDIF.
*    ENDTRY.
*  ENDIF.
*ENDFORM.                    " file_upload

*&---------------------------------------------------------------------*
*&      Form  directory_list
*&---------------------------------------------------------------------*
form DIRECTORY_LIST tables CT_FILE_NAME     type STRING_TABLE
                     using IV_DIRECTORY_NAME type STRING.

  data: LT_FILE_TABLE type table of FILE_INFO,
        LS_FILE_TABLE like line of LT_FILE_TABLE.
  data: LT_DIR_LIST type table of EPSFILI,
        LS_DIR_LIST like line of LT_DIR_LIST.
  data: LV_DIR_NAME     type EPSF-EPSDIRNAM.
  data: LV_COUNT        type I,
        LV_FILE_COUNTER type EPSF-EPSFILSIZ.                "#EC NEEDED
  data: LV_FILENAME     type STRING.
  data: LV_STR_LEN   type I,
        LV_LAST_CHAR type C.

  refresh CT_FILE_NAME.

  LV_STR_LEN = STRLEN( IV_DIRECTORY_NAME ).
  if LV_STR_LEN >= 1.
    LV_STR_LEN = LV_STR_LEN - 1.
    LV_LAST_CHAR = IV_DIRECTORY_NAME+LV_STR_LEN(1).
  endif.

* ------- Get directory file list in presentation server --------------
  if P_XPCIN = 'X'.
    call method CL_GUI_FRONTEND_SERVICES=>DIRECTORY_LIST_FILES
      exporting
        DIRECTORY                   = IV_DIRECTORY_NAME
*       filter                      = '*.*'
        FILES_ONLY                  = 'X'
      changing
        FILE_TABLE                  = LT_FILE_TABLE
        COUNT                       = LV_COUNT
      exceptions
        CNTL_ERROR                  = 1
        DIRECTORY_LIST_FILES_FAILED = 2
        WRONG_PARAMETER             = 3
        ERROR_NO_GUI                = 4
        NOT_SUPPORTED_BY_GUI        = 5
        others                      = 6.
    if SY-SUBRC <> 0.
      message id SY-MSGID type SY-MSGTY number SY-MSGNO
                 with SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    else.
      loop at LT_FILE_TABLE into LS_FILE_TABLE.
        if LV_LAST_CHAR = '\'.
          concatenate IV_DIRECTORY_NAME LS_FILE_TABLE-FILENAME into LV_FILENAME.
        else.
          concatenate IV_DIRECTORY_NAME '\' LS_FILE_TABLE-FILENAME into LV_FILENAME.
        endif.
        append LV_FILENAME to CT_FILE_NAME.
      endloop.
    endif.

* ------- Get directory file list in application server ----------------
  elseif P_XAPIN = 'X'.
    LV_DIR_NAME = IV_DIRECTORY_NAME.
    call function 'EPS_GET_DIRECTORY_LISTING'
      exporting
        DIR_NAME               = LV_DIR_NAME
      importing
        FILE_COUNTER           = LV_FILE_COUNTER
*       ERROR_COUNTER          =
      tables
        DIR_LIST               = LT_DIR_LIST
      exceptions
        INVALID_EPS_SUBDIR     = 1
        SAPGPARAM_FAILED       = 2
        BUILD_DIRECTORY_FAILED = 3
        NO_AUTHORIZATION       = 4
        READ_DIRECTORY_FAILED  = 5
        TOO_MANY_READ_ERRORS   = 6
        EMPTY_DIRECTORY_LIST   = 7
        others                 = 8.
    if SY-SUBRC <> 0.
      if not SY-MSGTY is initial.
        message id SY-MSGID type SY-MSGTY number SY-MSGNO
                with SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      endif.
    else.
      loop at LT_DIR_LIST into LS_DIR_LIST.
        if LV_LAST_CHAR = '/'.
          concatenate LV_DIR_NAME LS_DIR_LIST-NAME into LV_FILENAME.
        else.
          concatenate LV_DIR_NAME '/' LS_DIR_LIST-NAME into LV_FILENAME.
        endif.
        append LV_FILENAME to CT_FILE_NAME.
      endloop.
    endif.

  endif.

endform.                    " directory_list
*&---------------------------------------------------------------------*
*&      Form  file_download
*&---------------------------------------------------------------------*
form FILE_DOWNLOAD.

  data: LV_COUNTER_TOTAL type NUM10,
        LV_FILENAME      type STRING.

** Get total number of lines in file
*  PERFORM get_counter_by_block USING space CHANGING lv_counter_total.

  if GV_APPSV = ABAP_TRUE.
* File in app. server is already opened and closing is needed
    perform CLOSE_FILE.
  endif.

  GS_SAVED_I030-REG     = CONST_REG_I030.
  GS_SAVED_I030-QTD_LIN = W_TOTAL.
  call function 'CONVERSION_EXIT_ALPHA_INPUT'
    exporting
      INPUT  = GS_SAVED_I030-QTD_LIN
    importing
      OUTPUT = GS_SAVED_I030-QTD_LIN.

  perform FORMAT_RECORD_TO_LINE using GS_SAVED_I030 changing GS_RESULT.

  "  PERFORM process_counter_i030 USING lv_counter_total.
  if GV_APPSV = ABAP_TRUE and not GV_OFFSET_I030 is initial.
    perform OPEN_FILE using GV_OFFSET_I030.
    transfer GS_RESULT to P_APOUT.
    if SY-SUBRC <> 0.
      message E027 with P_APOUT.
    endif.
    perform CLOSE_FILE.
  endif.
  if GV_LCLSV = ABAP_TRUE and not GV_INDEX_I030 is initial.
    modify GT_RESULT from GS_RESULT index GV_INDEX_I030.
  endif.

*  PERFORM process_counter_j900 USING lv_counter_total.

  if GV_LCLSV = ABAP_TRUE.
    LV_FILENAME = P_PCOUT.
    call function 'GUI_DOWNLOAD'
      exporting
        FILENAME         = LV_FILENAME
        FILETYPE         = 'ASC'
      tables
        DATA_TAB         = GT_RESULT
      exceptions
        FILE_NOT_FOUND   = 1
        FILE_WRITE_ERROR = 2.
    case SY-SUBRC.
      when 0. "No log storage (would cause red flag)
        message S031 with LV_FILENAME. "p_pcout
      when 1.
        message E026 with LV_FILENAME SPACE.
      when 2.
        message E027 with LV_FILENAME.
    endcase.
    free GT_RESULT.
  endif.

  if GV_APPSV = ABAP_TRUE. "no errors found until here, so...
    message S031 with P_APOUT.
  endif.

endform.                    " file_download
*&---------------------------------------------------------------------*
*&      Form  open_file
*&---------------------------------------------------------------------*
form OPEN_FILE using IV_POSITION type TP_FILE_POSITION.
  data: LV_MSG      type STRING.
  if IV_POSITION is initial.
* Create new file
    open dataset P_APOUT in text mode for output
                         message LV_MSG
                         encoding default
                         ignoring conversion errors.
    if SY-SUBRC ne 0.
      message E026 with P_APOUT LV_MSG.
    endif.
  else.
* Update app. server file for reprocessing
    open dataset P_APOUT in text mode for update
                         at position IV_POSITION
                         message LV_MSG
                         encoding default
                         ignoring conversion errors.
    if SY-SUBRC ne 0.
      message E033 with P_APOUT LV_MSG.
    endif.
  endif.
endform.                    " OPEN_FILE
*&---------------------------------------------------------------------*
*&      Form  close_file
*&---------------------------------------------------------------------*
form CLOSE_FILE.
  close dataset P_APOUT.
  if SY-SUBRC <> 0.
    message E039 with P_APOUT.
  endif.
endform.                    "close_file


*&      Form  select_and_buffer_branch
*&---------------------------------------------------------------------*
form SELECT_AND_BUFFER_BRANCH using IV_BUKRS type BUKRS.

  data: LV_BRANCH     type J_1BBRANC_,
        LT_ADRC       type table of ADRC,
        LV_STATE_INSC type J_1BBRANCH-STATE_INSC,
        LV_MUNIC_INSC type J_1BBRANCH-MUNIC_INSC,
        LV_TAXJURCODE type ADRC-TAXJURCODE,
        LV_TEXT       type STRING,
        LV_LENGTH     type I.

* Get the business place that represents the headquarter of the company
  call function 'J_1B_BRANCH_DETERMINE'
    exporting
      COMPANY                  = IV_BUKRS
    importing
      BRANCH                   = LV_BRANCH
    exceptions
      BRANCH_NOT_FOUND         = 1
      PLANT_NOT_FOUND          = 2
      VALUATION_AREA_NOT_FOUND = 3
      COMPANY_NOT_FOUND        = 4
      others                   = 5.
  if SY-SUBRC <> 0.
    clear LV_TEXT.
    message E021 into LV_TEXT.
    if not LV_TEXT is initial.
      call function 'J_1BECD_LOG_ADD_MESSAGE'
        exporting
          IV_EXCNO = GV_EXCNO.
    endif.
  endif.

* Get general data of the headquater from the business place entity
  call function 'J_1BREAD_BRANCH_DATA'
    exporting
      BRANCH            = LV_BRANCH
      BUKRS             = IV_BUKRS
    importing
      BRANCH_DATA       = GS_BRANCH_DATA
      CGC_NUMBER        = GV_CNPJ
    exceptions
      BRANCH_NOT_FOUND  = 1
      ADDRESS_NOT_FOUND = 2
      COMPANY_NOT_FOUND = 3
      others            = 4.
  if SY-SUBRC <> 0.
    clear LV_TEXT.
    message E035 into LV_TEXT with LV_BRANCH.
    if not LV_TEXT is initial.
      call function 'J_1BECD_LOG_ADD_MESSAGE'
        exporting
          IV_EXCNO = GV_EXCNO.
    endif.
  endif.

  LV_STATE_INSC = GS_BRANCH_DATA-STATE_INSC.
  LV_MUNIC_INSC = GS_BRANCH_DATA-MUNIC_INSC.

* remove punctuation marks in fields
* gs_branch_data-state_insc and gs_branch_data-munic_insc

  if LV_STATE_INSC is not initial.
    translate LV_STATE_INSC using '- + * / \ . : ; , _ ( ) [ ] # < > '.
    condense LV_STATE_INSC no-gaps.
    move LV_STATE_INSC to GS_BRANCH_DATA-STATE_INSC.
  endif.

  if LV_MUNIC_INSC is not initial.
    translate LV_MUNIC_INSC using '- + * / \ . : ; , _ ( ) [ ] # < > '.
    condense LV_MUNIC_INSC no-gaps.
    move LV_MUNIC_INSC to GS_BRANCH_DATA-MUNIC_INSC.
  endif.

* Get address data of the headquarter
  call function 'ADDR_SELECT_ADRC_SINGLE'
    exporting
      ADDRNUMBER = GS_BRANCH_DATA-ADRNR
    tables
      ET_ADRC    = LT_ADRC
    exceptions
      others     = 0. "no address data will be shown

  read table LT_ADRC into GS_ADRC index 1.

* Field NOME in Reg.0000, I030 and J900 can be filled via BAdI
  if P_BADI = CONST_BADI.
    if GREF_BADI_J_1BECD is bound.
      call badi GREF_BADI_J_1BECD->FILL_COMPANY_NAME
        exporting
          IS_BRANCH_DATA = GS_BRANCH_DATA
          IS_ADRC        = GS_ADRC
        importing
          EV_NOME        = GV_NOME.
    endif.
  endif.

* remove punctuation marks in field gs_adrc-taxjurcode+3(7)
  LV_TAXJURCODE = GS_ADRC-TAXJURCODE+3(7).
  if LV_TAXJURCODE is not initial.
    translate LV_TAXJURCODE using '- + * / \ . : ; , _ ( ) [ ] # < > '.
    condense LV_TAXJURCODE no-gaps.
* insert leading zeros in field gs_adrc-taxjurcode+3(7)
    clear LV_LENGTH.
    LV_LENGTH = 7 - STRLEN( LV_TAXJURCODE ).
    do LV_LENGTH times.
      concatenate '0' LV_TAXJURCODE into LV_TAXJURCODE.
    enddo.
    move LV_TAXJURCODE to GS_ADRC-TAXJURCODE+3(7).
  endif.

endform.                    "select_and_buffer_branch

*&---------------------------------------------------------------------*
*&      Form  process_reg_0000
*&---------------------------------------------------------------------*
form PROCESS_REG_0000.
  data: IV_POPER  type POPER.
  call function 'FAGL_GET_INFO_FROM_LEDGER'                 "1410166
    exporting                                             "1410166
*     I_BUDAT        =
      I_RLDNR        = S_RLDNR-LOW                  "1410166
      I_BUKRS        = P_BUKRS                      "1410166
    importing                                             "1410166
      E_PERIV        = GV_RLDNR_PERIV               "1410166
*     E_POPER        =
*     E_GJAHR        =
    exceptions                                            "1410166
      NO_INFO_FOUND  = 1                            "1410166
      ERROR_IN_SETUP = 2                            "1410166
      others         = 3.                           "1410166
  if SY-SUBRC <> 0.
    GV_RLDNR_PERIV = GS_T001-PERIV.                         "1410166
  endif.

  IV_POPER = S_MONAT-LOW.
  call function 'FIRST_DAY_IN_PERIOD_GET'
    exporting
      I_GJAHR        = P_GJAHR
*     I_MONMIT       = 00
      I_PERIV        = GV_RLDNR_PERIV
      I_POPER        = IV_POPER
    importing
      E_DATE         = GS_FIN_CONTROL-DTINI
    exceptions
      INPUT_FALSE    = 1
      T009_NOTFOUND  = 2
      T009B_NOTFOUND = 3
      others         = 4.
  if SY-SUBRC = 0.

  endif.
  IV_POPER = S_MONAT-HIGH.
  call function 'LAST_DAY_IN_PERIOD_GET'
    exporting
      I_GJAHR        = P_GJAHR
      I_PERIV        = GV_RLDNR_PERIV
      I_POPER        = IV_POPER
    importing
      E_DATE         = GS_FIN_CONTROL-DTFIN
    exceptions
      INPUT_FALSE    = 1
      T009_NOTFOUND  = 2
      T009B_NOTFOUND = 3
      others         = 4.
  if SY-SUBRC = 0.

  endif.

  GS_0000-REG     = '0000'.
  GS_0000-LECD    = text-900.
  GS_0000-DT_INI = GS_FIN_CONTROL-DTINI.
  GS_0000-DT_FIN = GS_FIN_CONTROL-DTFIN.
  if not GV_NOME is initial.
    GS_0000-NOME    = GV_NOME.
  else.
    GS_0000-NOME    = GS_BRANCH_DATA-NAME.
  endif.
  GS_0000-CNPJ    = GV_CNPJ.
  GS_0000-UF      = GS_ADRC-REGION.
  GS_0000-IE      = GS_BRANCH_DATA-STATE_INSC.
  GS_0000-COD_MUN = GS_ADRC-TAXJURCODE+3(7).                "+8(7)
  GS_0000-IM      = GS_BRANCH_DATA-MUNIC_INSC.
  GS_0000-IND_SIT_ESP  = P_INDESP.
  GS_0000-IND_NIRE        = P_INDNIR.                       "1909036
  GS_0000-IND_FIN_ESC     = P_ESC.                          "1909036
  GS_0000-COD_HASH_SUB    = P_NIRHAS.                       "1909036
  GS_0000-NIRE_SUBST      = P_NIRSUB.                       "1909036
  GS_0000-IND_SIT_INI_PER = P_INIPER.                       "1909036
  GS_0000-IND_EMP_GRD_PRT = P_INDEGP.                       "1961806
  GS_0000-TIP_ECD         = P_TIPECD.                       "2154499
  GS_0000-COD_SCP         = P_CODSCP.                       "2154499
  GS_0000-IDENT_MF        = P_IDNTMF.                       "2272317
  GS_0000-IND_ESC_CONS    = P_CONS.
  if GREF_BADI_J_1BECD is bound.                            "1909036
    call badi GREF_BADI_J_1BECD->FILL_REGISTER_0000         "1909036
      changing                                              "1909036
        CS_REG_0000 = GS_0000.                              "1909036
  endif.                                                    "1909036

  append GS_0000 to GT_0000.

endform.              " process_reg_0000
*&---------------------------------------------------------------------*
*&      Form  F_BLOCO_0
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form F_BLOCO_0 .

  data: LS_VERSIONS_0000   type TP_VERSIONS.
  data: LS_RECORD_0000_VERSION type ref to DATA.

  field-symbols: <LS_RECORD_0000_VERSION> type ANY,
                 <COMPONENT_VERSION>      type ANY.

  clear:  W_TOTAL_I990,  W_TOTAL_J990,  W_TOTAL_9990, W_TOTAL_J900, W_TOTAL_0990.
* Select Business Place Data
  "0000
  perform SELECT_AND_BUFFER_BRANCH using P_BUKRS.
  perform PROCESS_REG_0000.
  if R_MOEDA = 'X'.
    GS_0000-IDENT_MF = 'S'.
  endif.

  "Verifica Versão do Layout 30.03.2017
  perform GET_LAYOUT_TYPE using GS_0000(4)
                                P_LAYOUT
                       changing LS_VERSIONS_0000.
  if LS_VERSIONS_0000-TYPE is not initial.
    create data LS_RECORD_0000_VERSION type (LS_VERSIONS_0000-TYPE).
    assign LS_RECORD_0000_VERSION->* to <LS_RECORD_0000_VERSION>.
  endif.
  "Fim Verifica

  if <LS_RECORD_0000_VERSION> is assigned.
    move-corresponding GS_0000 to <LS_RECORD_0000_VERSION>.
    perform FORMAT_RECORD_TO_LINE using <LS_RECORD_0000_VERSION>
                               changing LS_RESULT.
  else.
    perform FORMAT_RECORD_TO_LINE using GS_0000
                               changing LS_RESULT.          "1909036
  endif.

  if GV_APPSV = ABAP_TRUE.
    transfer LS_RESULT to P_APOUT.
  else.
    GS_RESULT = LS_RESULT.
    append GS_RESULT to GT_RESULT.
  endif.

  GS_0001-REG     = '0001'.
  GS_0001-IND_DAD = CONST_BLOCK_WITH.
  append GS_0001 to GT_0001.
  perform FORMAT_RECORD_TO_LINE using GS_0001
                                  changing LS_RESULT.       "1909036
  if GV_APPSV = ABAP_TRUE.
    transfer LS_RESULT to P_APOUT.
  else.
    GS_RESULT = LS_RESULT.
    append GS_RESULT to GT_RESULT.
  endif.

* Select Government Entity Codes
  select * from J_1BECD_CUST01 into table GT_GOV_CODE
    where BUKRS = P_BUKRS.

  GS_0990-QTD_LIN_0 = 2.
  loop at GT_GOV_CODE into GS_GOV_CODE.
    add 1 to GS_0990-QTD_LIN_0.
    GS_0007-REG         = '0007'.
    GS_0007-COD_ENT_REF = GS_GOV_CODE-COD_ENT_REF.
    GS_0007-COD_INSCR   = GS_GOV_CODE-COD_INSCR.
    append GS_0007 to GT_0007.
    perform FORMAT_RECORD_TO_LINE using GS_0007
                                  changing LS_RESULT.       "1909036
    if GV_APPSV = ABAP_TRUE.
      transfer LS_RESULT to P_APOUT.
    else.
      GS_RESULT = LS_RESULT.
      append GS_RESULT to GT_RESULT.
    endif.
  endloop.

  perform FILL_FIN_CONTROL.
* Select Inter-Company Data
  select * from J_1BECD_CUST06 into table GT_PARTICIPANT
    where BUKRS = P_BUKRS
      and ( ( DT_INI_REL >= GS_FIN_CONTROL-DTINI and
            DT_INI_REL <= GS_FIN_CONTROL-DTFIN )
      or  ( DT_INI_REL <  GS_FIN_CONTROL-DTINI and
            DT_FIN_REL = 0 )
      or  ( DT_INI_REL <  GS_FIN_CONTROL-DTINI and
            DT_FIN_REL > GS_FIN_CONTROL-DTINI ) ).

  "0150
  data: LS_T001     type T001,
        LV_COD_PAIS type J_1BEFD_COUNTRY-COD_PAIS,
        LT_ADRC_CC  type table of ADRC,
        LS_ADRC_CC  type ADRC.

  data: LV_BUKRS_LNAME type J_1BECD_0150_2_S-NOME.

  clear GT_0150.
  clear LV_COD_PAIS.
  loop at GT_PARTICIPANT into GS_PARTICIPANT.
    clear GS_0150.
    GS_0150-COD_PART    = GS_PARTICIPANT-BUKRS_REL.

* Select Company Code Data
    call function 'FI_COMPANY_CODE_DATA'
      exporting
        I_BUKRS      = GS_PARTICIPANT-BUKRS_REL
      importing
        E_T001       = LS_T001
      exceptions
        SYSTEM_ERROR = 1
        others       = 2.
    if SY-SUBRC <> 0. "FI_COMPANY_CODE_DATA issues message
      call function 'J_1BECD_LOG_ADD_MESSAGE'
        exporting
          IV_EXCNO = GV_EXCNO.
    endif.

* Select COD_PAIS from table J_1BEFD_COUNTRY
    select single COD_PAIS from J_1BEFD_COUNTRY into LV_COD_PAIS
        where LAND1 = LS_T001-LAND1.

* Related participant is outside Brazil - read Company Code data
    if LS_T001-LAND1 <> 'BR' or  LS_T001-BUKRS = '0004'. "Exceção
      clear LV_BUKRS_LNAME.
      if P_BADI = CONST_BADI.
        if GREF_BADI_J_1BECD is bound.
* Get address data of the company code
          clear: LT_ADRC_CC, LS_ADRC_CC.
          call function 'ADDR_SELECT_ADRC_SINGLE'
            exporting
              ADDRNUMBER = LS_T001-ADRNR
            tables
              ET_ADRC    = LT_ADRC_CC
            exceptions
              others     = 0. "no address data will be shown
          read table LT_ADRC_CC into LS_ADRC_CC index 1.
          call badi GREF_BADI_J_1BECD->FILL_COMPANY_NAME
            exporting
              IS_T001 = LS_T001
              IS_ADRC = LS_ADRC_CC
            importing
              EV_NOME = LV_BUKRS_LNAME.
        endif.
      endif.
      if not LV_BUKRS_LNAME is initial. "Check if long name is needed
        GS_0150-NOME        = LV_BUKRS_LNAME.
      else.
        GS_0150-NOME        = LS_T001-BUTXT. "Short name is the default
      endif.
      GS_0150-COD_PAIS    = LV_COD_PAIS.
*     all other fields remain empty
    else.
* Related participant is inside Brazil- read Business Place data
      perform SELECT_AND_BUFFER_BRANCH_REL using GS_PARTICIPANT-BUKRS_REL.
      if not GV_NOME_REL is initial. "Check if long name is needed
        GS_0150-NOME        = GV_NOME_REL.
      else.
        GS_0150-NOME        = GS_BRANCH_REL_DATA-NAME. "Short name is the default
      endif.
      GS_0150-COD_PAIS    = LV_COD_PAIS.
      GS_0150-CNPJ        = GV_CNPJ_REL.
*     gs_0150-cpf         =
*     gs_0150-nit         =
      GS_0150-UF          = GS_ADRC_REL-REGION.
      GS_0150-IE          = GS_BRANCH_REL_DATA-STATE_INSC.
*     gs_0150-ie_st       =
      GS_0150-COD_MUN     = GS_ADRC_REL-TAXJURCODE+3(7).
      GS_0150-IM          = GS_BRANCH_REL_DATA-MUNIC_INSC.
*     gs_0150-suframa     =
    endif.
    collect GS_0150 into GT_0150.
  endloop.

* The entire register can be reprocessed via exit
  if P_BADI = CONST_BADI.
    if GREF_BADI_J_1BECD is bound.
      call badi GREF_BADI_J_1BECD->FILL_REGISTER_0150
        exporting
          IV_BUKRS    = P_BUKRS
          IT_REG_0150 = GT_0150
        importing
          ET_REG_0150 = GT_0150.
    endif.
  endif.

* Per entry in register 0150, N entries in register 0180 are possible
  loop at GT_0150 into GS_0150.
    add 1 to GS_0990-QTD_LIN_0.
    GS_0150-REG = CONST_REG_0150.
    perform FORMAT_RECORD_TO_LINE using GS_0150
                              changing LS_RESULT.           "1909036
    if GV_APPSV = ABAP_TRUE.
      transfer LS_RESULT to P_APOUT.
    else.
      GS_RESULT = LS_RESULT.
      append GS_RESULT to GT_RESULT.
    endif.
    "0180
    clear GT_0180.
    clear GS_PARTICIPANT.
    loop at GT_PARTICIPANT into GS_PARTICIPANT
      where BUKRS_REL = GS_0150-COD_PART.
      add 1 to GS_0990-QTD_LIN_0.
      clear GS_0180.
      GS_0180-REG         = CONST_REG_0180.
      GS_0180-COD_REL     = GS_PARTICIPANT-COD_REL.
      GS_0180-DT_INI_REL  = GS_PARTICIPANT-DT_INI_REL.
      GS_0180-DT_FIN_REL  = GS_PARTICIPANT-DT_FIN_REL.
      perform FORMAT_RECORD_TO_LINE using GS_0180
                               changing LS_RESULT.          "1909036
      if GV_APPSV = ABAP_TRUE.
        transfer LS_RESULT to P_APOUT.
      else.
        GS_RESULT = LS_RESULT.
        append GS_RESULT to GT_RESULT.
      endif.
    endloop.
  endloop.

  GS_0990-REG       = '0990'.
  add 1 to GS_0990-QTD_LIN_0.
  perform FORMAT_RECORD_TO_LINE using GS_0990
                                changing LS_RESULT.         "1909036
  if GV_APPSV = ABAP_TRUE.
    transfer LS_RESULT to P_APOUT.
  else.
    GS_RESULT = LS_RESULT.
    append GS_RESULT to GT_RESULT.
  endif.



endform.                    " F_BLOCO_0

*&---------------------------------------------------------------------*
*&      Form  select_and_buffer_branch_rel
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->IV_BUKRS   text
*----------------------------------------------------------------------*
form SELECT_AND_BUFFER_BRANCH_REL using IV_BUKRS type BUKRS.

  data: LV_BRANCH_REL     type J_1BBRANC_,
        LT_ADRC_REL       type table of ADRC,
        LV_STATE_INSC_REL type J_1BBRANCH-STATE_INSC,
        LV_MUNIC_INSC_REL type J_1BBRANCH-MUNIC_INSC,
        LV_TAXJURCODE_REL type ADRC-TAXJURCODE,
        LV_TEXT           type STRING,
        LV_LENGTH         type I.

* Get the business place that represents the headquarter of the company
  call function 'J_1B_BRANCH_DETERMINE'
    exporting
      COMPANY                  = IV_BUKRS
    importing
      BRANCH                   = LV_BRANCH_REL
    exceptions
      BRANCH_NOT_FOUND         = 1
      PLANT_NOT_FOUND          = 2
      VALUATION_AREA_NOT_FOUND = 3
      COMPANY_NOT_FOUND        = 4
      others                   = 5.
  if SY-SUBRC <> 0.
    clear LV_TEXT.
    message E021 into LV_TEXT.
    if not LV_TEXT is initial.
      call function 'J_1BECD_LOG_ADD_MESSAGE'
        exporting
          IV_EXCNO = GV_EXCNO.
    endif.
  endif.

* Get general data from the business place entity
  call function 'J_1BREAD_BRANCH_DATA'
    exporting
      BRANCH            = LV_BRANCH_REL
      BUKRS             = IV_BUKRS
    importing
      BRANCH_DATA       = GS_BRANCH_REL_DATA
      CGC_NUMBER        = GV_CNPJ_REL
    exceptions
      BRANCH_NOT_FOUND  = 1
      ADDRESS_NOT_FOUND = 2
      COMPANY_NOT_FOUND = 3
      others            = 4.
  if SY-SUBRC <> 0.
    clear LV_TEXT.
    message E035 into LV_TEXT with LV_BRANCH_REL.
    if not LV_TEXT is initial.
      call function 'J_1BECD_LOG_ADD_MESSAGE'
        exporting
          IV_EXCNO = GV_EXCNO.
    endif.
  endif.

  LV_STATE_INSC_REL = GS_BRANCH_REL_DATA-STATE_INSC.
  LV_MUNIC_INSC_REL = GS_BRANCH_REL_DATA-MUNIC_INSC.

* remove punctuation marks in fields
* gs_branch_data-state_insc and gs_branch_data-munic_insc

  if LV_STATE_INSC_REL is not initial.
    translate LV_STATE_INSC_REL using '- + * / \ . : ; , _ ( ) [ ] # < > '.
    condense LV_STATE_INSC_REL no-gaps.
    move LV_STATE_INSC_REL to GS_BRANCH_REL_DATA-STATE_INSC.
  endif.

  if LV_MUNIC_INSC_REL is not initial.
    translate LV_MUNIC_INSC_REL using '- + * / \ . : ; , _ ( ) [ ] # < > '.
    condense LV_MUNIC_INSC_REL no-gaps.
    move LV_MUNIC_INSC_REL to GS_BRANCH_REL_DATA-MUNIC_INSC.
  endif.

* Get address data of the business place
  call function 'ADDR_SELECT_ADRC_SINGLE'
    exporting
      ADDRNUMBER = GS_BRANCH_REL_DATA-ADRNR
    tables
      ET_ADRC    = LT_ADRC_REL
    exceptions
      others     = 0. "no address data will be shown

  read table LT_ADRC_REL into GS_ADRC_REL index 1.

* Field NOME in Register 0150 can be filled via BAdI
  if P_BADI = CONST_BADI.
    if GREF_BADI_J_1BECD is bound.
      call badi GREF_BADI_J_1BECD->FILL_COMPANY_NAME
        exporting
          IS_BRANCH_DATA = GS_BRANCH_REL_DATA
          IS_ADRC        = GS_ADRC_REL
        importing
          EV_NOME        = GV_NOME_REL.
    endif.
  endif.

* remove punctuation marks in field gs_adrc-taxjurcode+3(7)
  LV_TAXJURCODE_REL = GS_ADRC_REL-TAXJURCODE+3(7).
  if LV_TAXJURCODE_REL is not initial.
    translate LV_TAXJURCODE_REL using '- + * / \ . : ; , _ ( ) [ ] # < > '.
    condense LV_TAXJURCODE_REL no-gaps.
* insert leading zeros in field gs_adrc-taxjurcode+3(7)
    clear LV_LENGTH.
    LV_LENGTH = 7 - STRLEN( LV_TAXJURCODE_REL ).
    do LV_LENGTH times.
      concatenate '0' LV_TAXJURCODE_REL into LV_TAXJURCODE_REL.
    enddo.
    move LV_TAXJURCODE_REL to GS_ADRC_REL-TAXJURCODE+3(7).
  endif.

endform.                    "select_and_buffer_branch_rel
*&---------------------------------------------------------------------*
*&      Form  GET_DOMAIN_TEXT
*&---------------------------------------------------------------------*
form GET_DOMAIN_TEXT  using IV_NAME  type DOMNAME
                            IV_VALUE type DOMVALUE_L
                   changing CV_TEXT  type VAL_TEXT.
  data: LV_SPRAS type DDLANGUAGE.

  clear CV_TEXT.

  LV_SPRAS      = SY-LANGU. "p_spra is not used (system object)

  call function 'C_DIC_DOMAIN_VALUE_TEXT_READ'
    exporting
      NAME      = IV_NAME
      SPRAS     = LV_SPRAS
      VALUE     = IV_VALUE
    importing
      TEXT      = CV_TEXT
    exceptions
      NOT_FOUND = 0
      others    = 0.

endform.                    " GET_DOMAIN_TEXT

*&---------------------------------------------------------------------*
*&      "1481772
*&---------------------------------------------------------------------*
form FILL_PL_AGLUTINATION using IS_ANTERIOR_RECORD type J_1BECD_I050_3_S.
* Identification of Agglutination Codes for P&L accounts (similar to I052)
  data: LS_I052 type J_1BECD_I052_4_S,
        LT_I052 type J_1BECD_I052_4_T.
  data: LS_ACCOUNT_LEVEL type J_1BECD_SINTETIC_ACC_S,
        LS_AGLUT_LEVEL   type TP_AGLUT_LEVEL,
        LV_LEVEL         type J_1BECD_SINTETIC_ACC_S-FSV_LEVEL.

  clear LT_I052. "local table (do not use gt_i052)

  clear LS_I052.

* Aglutination code is the FSV level code
  loop at GT_ACCOUNT_LEVEL into LS_ACCOUNT_LEVEL.
    LS_I052-COD_AGL = LS_ACCOUNT_LEVEL-ACCOUNT_NR.
    append LS_I052 to LT_I052.
  endloop.

* Buffer agglutination codes with accounts for use in block J
  if GS_FIN_CONTROL-TOTAL_ACC_IND_J = ABAP_TRUE.
* (otherwise, no need to buffer this info - J100,J150 won't be filled)
    loop at LT_I052 into LS_I052.
      LV_LEVEL = SY-TABIX. "from loop immediatly before
      read table GT_ACCOUNT_LEVEL into LS_ACCOUNT_LEVEL
        with key FSV_LEVEL = LV_LEVEL.
      if SY-SUBRC ne 0.
* exit loop due to inconsistency - more aglutination codes than sintetic
* accounts in the analytical account higher levels
* totals are displayed only at node level in FSV (sintetic accounts).
        exit. "loop
      endif.
      clear LS_AGLUT_LEVEL.
      LS_AGLUT_LEVEL-AGLUT_CODE    = LS_I052-COD_AGL.
      LS_AGLUT_LEVEL-LEVEL         = LS_ACCOUNT_LEVEL-FSV_LEVEL.
      LS_AGLUT_LEVEL-ACCOUNT_NR    = IS_ANTERIOR_RECORD-COD_CTA.
      LS_AGLUT_LEVEL-DESCRIPTION   = LS_ACCOUNT_LEVEL-DESCRIPTION.
      LS_AGLUT_LEVEL-SPED_INT_CODE = IS_ANTERIOR_RECORD-COD_NAT.
      if IS_ANTERIOR_RECORD-COD_NAT = CONST_IC_ACC_NET_EQUITY.
        append LS_AGLUT_LEVEL to GT_BAL_AGLUT_LEVEL.
      endif. "no other possibility here
    endloop.
  endif.

endform.                    " fill_pl_aglutination

*&---------------------------------------------------------------------*
*&      Form  select_and_buffer_fsv
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->IV_FSV     text
*      -->IV_KTOPL   text
*      -->IV_SPRA    text
*----------------------------------------------------------------------*
form SELECT_AND_BUFFER_FSV using IV_FSV   type J_1BECD_VERSN
                                 IV_KTOPL type KTOPL
                                 IV_SPRA  type J_1BECD_LANGU.

  data: LT_RF011Z type table of RF011Z,
        LT_RF011P type table of RF011P,
        LT_RF011S type table of RF011S,
        LT_RF011Q type table of RF011Q.

  data: LS_FSV           type TP_FSV,
        LT_BALANCE       type table of TP_FSV_STRUC,
        LS_BALANCE       type TP_FSV_STRUC,
        LV_SPED_INT_CODE like LS_FSV-SPED_INT_CODE,
        LV_INDEX         type SYINDEX.

  data: begin of LS_SUP_LEVEL, "all higher nodes of a FSV node
          LEVEL  type TP_FSV-LEVEL,
          ACC_NR type TP_FSV-SUPERIOR_ACC_NR,
        end of LS_SUP_LEVEL.
  data: LT_SUP_LEVEL like table of
          LS_SUP_LEVEL,
        LV_SUP_LEVEL like LS_FSV-LEVEL.

  field-symbols: <FS_RF011P>  type RF011P,
                 <FS_RF011Z>  type RF011Z,
                 <FS_RF011S>  type RF011S,
                 <FS_RF011Q>  type RF011Q,
                 <FS_BALANCE> type TP_FSV_STRUC,
                 <FS_SKA1>    type TP_SKA1.

  data: LR_SAKNR type range of SKA1-SAKNR, "Range for G/L Account Number
        LS_SAKNR like line of LR_SAKNR.

* --- Global table to store FSV and counter
  refresh GT_FSV.
  clear GV_FSV_CNT.
  if IV_FSV is initial.
    return.
  endif.

* Check authorization for FSV (RFBILA00)
  authority-check object 'F_T011'
    id 'VERSN' field IV_FSV
    id 'ACTVT' field '03'.
  if SY-SUBRC <> 0.
    message E031(FE).
  endif.

* --- Selecting FSV and texts
  call function 'FI_IMPORT_BALANCE_SHEET_POS'
    exporting
      VERSION           = IV_FSV
    tables
      I011Z             = LT_RF011Z
      X011P             = LT_RF011P
      X011S             = LT_RF011S
    exceptions
      NEW_BALANCE_SHEET = 1
      others            = 2.
  if SY-SUBRC <> 0. "FI_IMPORT_BALANCE_SHEET_POS issues message
    call function 'J_1BECD_LOG_ADD_MESSAGE'
      exporting
        IV_EXCNO = GV_EXCNO.
    return.  "simply leave the form
  endif.

  call function 'FI_IMPORT_BALANCE_SHEET_TEXT'
    exporting
      SPRACHE        = IV_SPRA
      VERSION        = IV_FSV
    tables
      X011Q          = LT_RF011Q
*     TSTAMP_TAB     =
    exceptions
      TEXT_NOT_FOUND = 1
      others         = 2.
  if SY-SUBRC <> 0. "FI_IMPORT_BALANCE_SHEET_TEXT issues message
    call function 'J_1BECD_LOG_ADD_MESSAGE'
      exporting
        IV_EXCNO = GV_EXCNO.
*   continue processing without texts
  endif.

  delete LT_RF011Q where TXTYP <> 'K'. "use 'item text' (K)
  sort LT_RF011Q by ERGSL. "for binary search access
  sort LT_RF011S by ERGSL. "for binary search access
* --- Build intermediate local internal table with FSV nodes
  loop at LT_RF011P assigning <FS_RF011P>.
    clear LS_BALANCE.
    LS_BALANCE-ERGSL = <FS_RF011P>-ERGSL.
    LS_BALANCE-STUFE = <FS_RF011P>-STUFE.
    read table LT_RF011Q assigning <FS_RF011Q> binary search
      with key ERGSL = <FS_RF011P>-ERGSL.
    if SY-SUBRC = 0.
      LS_BALANCE-TXT45 = <FS_RF011Q>-TXT45.
    endif.
    read table LT_RF011S assigning <FS_RF011S> binary search
      with key ERGSL = <FS_RF011P>-ERGSL.
    if SY-SUBRC = 0.
      LS_BALANCE-SETNR = <FS_RF011S>-SETNR.
    endif.
    append LS_BALANCE to LT_BALANCE.
  endloop.
  free: LT_RF011P, LT_RF011Q, LT_RF011S.

* KTOPL should be the same as the SKA1 selection
  delete LT_RF011Z where KTOPL <> IV_KTOPL.

  sort LT_BALANCE by SETNR. "define order as needed for output
* --- Produce result combining FSV nodes with SKA1 info
  loop at LT_BALANCE assigning <FS_BALANCE>.
    perform GET_SPED_INT_CODE using <FS_BALANCE>-STUFE
                                    <FS_BALANCE>-ERGSL
                           changing LV_SPED_INT_CODE.
*... Sintetic accounts - hierarchy nodes
    clear LS_FSV.
    LS_FSV-SPED_INT_CODE    = LV_SPED_INT_CODE.
    LS_FSV-LEVEL            = <FS_BALANCE>-STUFE.
    LS_FSV-ACCOUNT_NR       = <FS_BALANCE>-ERGSL.
    LV_SUP_LEVEL            = LS_FSV-LEVEL - 1.
    read table LT_SUP_LEVEL
      into LS_SUP_LEVEL with key LEVEL = LV_SUP_LEVEL.
    if SY-SUBRC = 0.
      LS_FSV-SUPERIOR_ACC_NR  = LS_SUP_LEVEL-ACC_NR.
    endif.
    LS_FSV-DESCRIPTION      = <FS_BALANCE>-TXT45.
    LS_FSV-INCLUSION_DATE   = GS_FIN_CONTROL-DTINI. "convention
*... Append Sintetic
    LS_FSV-IS_ANALY_SINT    = CONST_ACC_SINTETIC.
    append LS_FSV to GT_FSV.
*... Local Level control
    read table LT_SUP_LEVEL into LS_SUP_LEVEL
      with key LEVEL = <FS_BALANCE>-STUFE.
    if SY-SUBRC = 0.
      LS_SUP_LEVEL-ACC_NR = <FS_BALANCE>-ERGSL.
      modify LT_SUP_LEVEL from LS_SUP_LEVEL index SY-TABIX.
    else.
      LS_SUP_LEVEL-LEVEL  = <FS_BALANCE>-STUFE.
      LS_SUP_LEVEL-ACC_NR = <FS_BALANCE>-ERGSL.
      " insert next level first keeps the table sorted by level desc
      insert LS_SUP_LEVEL into LT_SUP_LEVEL index 1.
    endif.
*... Analytical accounts
    loop at LT_RF011Z assigning <FS_RF011Z>
      where ERGSO = <FS_BALANCE>-ERGSL or "debit
            ERGHB = <FS_BALANCE>-ERGSL.   "credit
      refresh LR_SAKNR.
      clear LS_SAKNR.
      LS_SAKNR-SIGN = 'I'.
      LS_SAKNR-OPTION = 'BT'.
      LS_SAKNR-LOW = <FS_RF011Z>-VONKT.
      LS_SAKNR-HIGH = <FS_RF011Z>-BILKT.
      append LS_SAKNR to LR_SAKNR.
      loop at GT_SKA1 assigning <FS_SKA1>
        where SAKNR in LR_SAKNR.         "accounts in one FSV interval
        clear LS_FSV.
        LS_FSV-SPED_INT_CODE = LV_SPED_INT_CODE.
        LS_FSV-LEVEL         = <FS_BALANCE>-STUFE + 1.
        LV_SUP_LEVEL         = LS_FSV-LEVEL - 1.
        read table LT_SUP_LEVEL
          into LS_SUP_LEVEL with key LEVEL = LV_SUP_LEVEL.
        if SY-SUBRC = 0.
          LS_FSV-SUPERIOR_ACC_NR = LS_SUP_LEVEL-ACC_NR.
        endif.
        perform SET_ANALYTICAL_ACC_INFO using <FS_SKA1> IV_KTOPL IV_SPRA
                                     changing LS_FSV.
*... Append Analytical
        append LS_FSV to GT_FSV.
        <FS_SKA1>-IN_FSV = ABAP_TRUE.
      endloop. "SKA1(validated also SKB1) - accounts inside an interval
    endloop. "RF011Z - intervals inside a node
  endloop.

* --- Process P&L accounts
  perform ADD_PL_ACCOUNTS.                                  "1481772

* --- Delete empty FSV nodes
  perform DELETE_EMPTY_FSV_NODES.

* --- Add accounts assig ned to Company, but not listed in FSV
  if P_FSIND = SPACE. "Show unassigned accounts
    clear LV_INDEX.
    loop at GT_SKA1 assigning <FS_SKA1> where IN_FSV = ABAP_FALSE.
      LV_INDEX = LV_INDEX + 1. "sequencial counter
      clear LS_FSV. "SPED_INT_CODE, LEVEL and SUPERIOR_ACC_NR remain empty
      perform SET_ANALYTICAL_ACC_INFO using <FS_SKA1> IV_KTOPL IV_SPRA
                                   changing LS_FSV.
*... Append Analytical (insert on top and keep existing order)
      insert LS_FSV into GT_FSV index LV_INDEX.
    endloop.
  endif.

* --- Keep a counter for the FSV lines found
  describe table GT_FSV lines GV_FSV_CNT.

endform.                    "select_and_buffer_fsv

*&---------------------------------------------------------------------*
*&      Form  add_pl_accounts
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form ADD_PL_ACCOUNTS.
  data: LS_FSV type TP_FSV,
        LV_IDX type SYTABIX.

  if not
    ( GS_FIN_CONTROL-TOTAL_ACC_IND_J = ABAP_TRUE and P_DOCTYP is initial ).
    return. "only add P&L accounts for Standard Year End Closing scenario
  endif.

* Two nodes have to be created for Net Result
  if GV_ERGPA is initial or GV_ERGAK is initial or GV_ERGPA = GV_ERGAK.
    return.
  endif.

* Add two dummy "analytical" accounts (but with a different indicator "P"/"L")
* One of these accounts will be loaded with J150 top node value, for calculation
* The nodes (Sintetical accounts are needed to be listed in I050)
  read table GT_FSV into LS_FSV
    with key ACCOUNT_NR    = GV_ERGAK "Net Result Loss Node
             IS_ANALY_SINT = CONST_ACC_SINTETIC.
  if SY-SUBRC = 0.
    LV_IDX = SY-TABIX + 1. "next line in tab
    LS_FSV-IS_ANALY_SINT = CONST_ACC_SPEC_LOSS. "special value
    LS_FSV-LEVEL = LS_FSV-LEVEL + 1.
    LS_FSV-SUPERIOR_ACC_NR = LS_FSV-ACCOUNT_NR.
    insert LS_FSV into GT_FSV index LV_IDX.
  endif.

  read table GT_FSV into LS_FSV
    with key ACCOUNT_NR    = GV_ERGPA "Net Result Profit Node
             IS_ANALY_SINT = CONST_ACC_SINTETIC.
  if SY-SUBRC = 0.
    LV_IDX = SY-TABIX + 1. "next line in tab
    LS_FSV-IS_ANALY_SINT = CONST_ACC_SPEC_PROFIT. "special value
    LS_FSV-LEVEL = LS_FSV-LEVEL + 1.
    LS_FSV-SUPERIOR_ACC_NR = LS_FSV-ACCOUNT_NR.
    insert LS_FSV into GT_FSV index LV_IDX.
  endif.

endform.                    " ADD_PL_ACCOUNTS

*&---------------------------------------------------------------------*
*&      Form  delete_empty_fsv_nodes
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form DELETE_EMPTY_FSV_NODES.

  data:
    LV_FOUND    type ABAP_BOOL value ABAP_FALSE, "no empty nodes as default
    LV_IDX      type SYINDEX,    "current line index
    LS_FSV_NEXT type TP_FSV.  "following line (next after current line)
  field-symbols: <LS_FSV> type TP_FSV. "current line being checked

* Condition to consider one FSV line an empty node:
* 1) Last line, in case it is Sintetic OR
* 2) Sintetic line that contains a following sintetic line at same
*    level or higher

  LV_IDX = 1.
  do.
    read table GT_FSV assigning <LS_FSV> index LV_IDX. "current line
    if SY-SUBRC <> 0.
      exit. "do (no more lines to process
    endif.
    LV_IDX = LV_IDX + 1. "increment counter before starting checks
    check <LS_FSV>-IS_ANALY_SINT = CONST_ACC_SINTETIC.
    read table GT_FSV into LS_FSV_NEXT index LV_IDX. "next line
    if SY-SUBRC <> 0.
      LV_FOUND = ABAP_TRUE.
      <LS_FSV>-IS_EMPTY = ABAP_TRUE.
      exit. "do (current line is last line being processed)
    endif.
    check LS_FSV_NEXT-IS_ANALY_SINT = CONST_ACC_SINTETIC and
          <LS_FSV>-LEVEL >= LS_FSV_NEXT-LEVEL.
    LV_FOUND = ABAP_TRUE.
    <LS_FSV>-IS_EMPTY = ABAP_TRUE.
  enddo.

* delete empty nodes found in this iteration and check
* recursively
  if LV_FOUND = ABAP_TRUE.
    delete GT_FSV where IS_EMPTY = ABAP_TRUE.
    perform DELETE_EMPTY_FSV_NODES.
  endif.

endform.                    " DELETE_EMPTY_FSV_NODES

*&---------------------------------------------------------------------*
*&      Form  GET_SPED_INT_CODE
*&---------------------------------------------------------------------*
form GET_SPED_INT_CODE  using    IV_STUFE type STUFE_F02E
                                 IV_ERGSL type ERGSL
                        changing CV_SPED_INT_CODE
                                   type J_1BECD_I050_3_S-COD_NAT.
*01   Contas de Ativo (Asset Accounts)
*02   Contas de Passivo (Liability Accounts)
*03   Patrimônio Líquido (Net Equity Accounts)
*04   Contas de Resultado (P+L Accounts)
*05   Contas de Compensação (Contra-Accounts)
*06   Outras (Others)
  data: LS_NODE_CODE type TP_NODE_CODE.

  if IV_STUFE = '01'.
    clear CV_SPED_INT_CODE. "empty node as default
  endif.

* Sped Internal Code control
* At each moment it contains the higher nodes as processed before plus
* the new refreshed nodes below
* Example: 1-2-3-4-2 (at this point level 2, 3 and 4 nodes are deleted)
* and level 2 is inserted with fresh info. Node 1 remains as it is still
* the valid top node in this hierarchy
  delete GT_NODE_CODE where FSV_LEVEL >= IV_STUFE.
  LS_NODE_CODE-FSV_LEVEL = IV_STUFE.
  LS_NODE_CODE-ERGSL     = IV_ERGSL.
  insert LS_NODE_CODE into GT_NODE_CODE index 1. "inner nodes first
  loop at GT_NODE_CODE into LS_NODE_CODE
    where ERGSL = CONST_IC_ACC_ASSET
       or ERGSL = CONST_IC_ACC_LIABILITY
       or ERGSL = CONST_IC_ACC_NET_EQUITY
       or ERGSL = CONST_IC_ACC_INCOME
       or ERGSL = CONST_IC_ACC_LOSSES
       or ERGSL = CONST_IC_ACC_OTHERS.
    CV_SPED_INT_CODE = LS_NODE_CODE-ERGSL.
    exit. "only first entry needed
  endloop.

endform.                    " GET_SPED_INT_CODE

*&---------------------------------------------------------------------*
*&      Form  set_analytical_acc_info
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->IS_SKA1    text
*      -->IV_KTOPL   text
*      -->IV_SPRA    text
*      -->CS_FSV     text
*----------------------------------------------------------------------*
form SET_ANALYTICAL_ACC_INFO  using  IS_SKA1  type TP_SKA1
                                     IV_KTOPL type KTOPL
                                     IV_SPRA  type J_1BECD_LANGU
                            changing CS_FSV   type TP_FSV.

  data: LS_SKAT         type SKAT,
        LV_ALTKT        type SKB1-ALTKT,                    "1408160
        LV_LTEXT        type SKAT-TXT50,                    "1408160
        ALTKT_NOT_FOUND type C,                             "1408160
        TEXT_NOT_FOUND  type C,                             "1408160
        LV_TEXT         type STRING.                        "1408160

  CS_FSV-IS_ANALY_SINT    = CONST_ACC_ANALYTICAL.
  CS_FSV-ACCOUNT_NR       = IS_SKA1-SAKNR.
  CS_FSV-INCLUSION_DATE   = IS_SKA1-ERDAT.

  call function 'READ_HAUPTBUCH_TEXT'
    exporting
      KONTENPLAN     = IV_KTOPL
      SACHKONTO      = IS_SKA1-SAKNR
      SPRACHE        = IV_SPRA
*     NO_BUFFER      =
    importing
      TEXT_WA        = LS_SKAT
    exceptions
      TEXT_NOT_FOUND = 1
      others         = 2.

  if SY-SUBRC = 0.
    CS_FSV-DESCRIPTION  = LS_SKAT-TXT50.
  endif.

  if P_ALTKT = 'X'.                                         "1408160
* --- Display alternative account number                    "1408160
    call function 'READ_SACHKONTO_ALTKT'                    "1408160
      exporting                                             "1408160
*       ALTKT_I         = ' '
        BUKRS           = P_BUKRS                     "1408160
        SAKNR           = IS_SKA1-SAKNR               "1408160
        SPRAS           = IV_SPRA                     "1408160
*       XMASS           = ' '
        XSKAN           = 'X'                         "1408160
        XTEXT           = 'X'                         "1408160
      importing                                             "1408160
        ALTKT           = LV_ALTKT                    "1408160
        ALTKT_NOT_FOUND = ALTKT_NOT_FOUND             "1408160
*       ALTKT_SAKAN     =
*       KTEXT           =
        LTEXT           = LV_LTEXT                    "1408160
        TEXT_NOT_FOUND  = TEXT_NOT_FOUND              "1408160
      exceptions                                             "1408160
        BUKRS_NOT_FOUND = 1                            "1408160
        SAKNR_NOT_FOUND = 2                            "1408160
        others          = 3.                           "1408160

    if ALTKT_NOT_FOUND <> 'X' and                           "1408160
       TEXT_NOT_FOUND <> 'X'.                               "1408160
*     everything ok
*     -------------
      CS_FSV-ALTKT        = LV_ALTKT.                       "1408160
      CS_FSV-DESCRIPTION  = LV_LTEXT.                       "1408160
    else.                                                   "1408160
      if ALTKT_NOT_FOUND = 'X'.                             "1408160
*       alternative account number not maintained
*       -----------------------------------------
        clear LV_TEXT.                                      "1408160
        message E041 into LV_TEXT with IS_SKA1-SAKNR P_BUKRS. "1408160
        if not LV_TEXT is initial.                          "1408160
          call function 'J_1BECD_LOG_ADD_MESSAGE'           "1408160
            exporting                                       "1408160
              IV_EXCNO = GV_EXCNO.                          "1408160
        endif.                                              "1408160
      endif.                                                "1408160

      if TEXT_NOT_FOUND = 'X'.                              "1408160
*       text of alternative account number not maintained
*       -------------------------------------------------
        clear LV_TEXT.                                      "1408160
        message E042 into LV_TEXT with LV_ALTKT GS_T001-KTOP2. "1408160
        if not LV_TEXT is initial.                          "1408160
          call function 'J_1BECD_LOG_ADD_MESSAGE'           "1408160
            exporting                                       "1408160
              IV_EXCNO = GV_EXCNO.                          "1408160
        endif.                                              "1408160
        CS_FSV-ALTKT = LV_ALTKT.                            "1408160
      endif.                                                "1408160
    endif.                                                  "1408160
  endif.                                                    "1408160
endform.                    " SET_ANALYTICAL_ACC_INFO

*&---------------------------------------------------------------------*
*&      Form  fill_callback
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->IV_LDBNODE   text
*      -->IV_F_GET     text
*      -->IV_F_GLATE   text
*      -->IV_FORM      text
*      -->CT_CALLBACK  text
*----------------------------------------------------------------------*
form FILL_CALLBACK using IV_LDBNODE  type LDBNODE
                         IV_F_GET    type LDBCALLGET
                         IV_F_GLATE  type LDBCALLATE
                         IV_FORM     type RSDSFORM
                changing CT_CALLBACK type LDBCB_TT.
  data: LS_CALLBACK like line of CT_CALLBACK.
  clear LS_CALLBACK.
  LS_CALLBACK-LDBNODE  = IV_LDBNODE.
  LS_CALLBACK-GET      = IV_F_GET.
  LS_CALLBACK-GET_LATE = IV_F_GLATE.
  LS_CALLBACK-CB_PROG  = GV_REPID.
  LS_CALLBACK-CB_FORM  = IV_FORM.
  append LS_CALLBACK to CT_CALLBACK.
endform.                    "fill_callback

*&---------------------------------------------------------------------*
*&      Form  fill_seltab
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->IV_SELNAME text
*      -->IV_KIND    text
*      -->IV_SIGN    text
*      -->IV_OPTION  text
*      -->IV_LOW     text
*      -->IV_HIGH    text
*      -->CT_SELTAB  text
*----------------------------------------------------------------------*
form FILL_SELTAB  using IV_SELNAME type RSSCR_NAME
                        IV_KIND    type RSSCR_KIND
                        IV_SIGN    type TVARV_SIGN
                        IV_OPTION  type TVARV_OPTI
                        IV_LOW     type TVARV_VAL
                        IV_HIGH    type TVARV_VAL
               changing CT_SELTAB  type RSPARAMS_TT.
  data: LS_SELTAB like line of CT_SELTAB.
  clear LS_SELTAB.
  LS_SELTAB-SELNAME = IV_SELNAME.
  LS_SELTAB-KIND    = IV_KIND.
  LS_SELTAB-SIGN    = IV_SIGN.
  LS_SELTAB-OPTION  = IV_OPTION.
  LS_SELTAB-LOW     = IV_LOW.
  LS_SELTAB-HIGH    = IV_HIGH.
  append LS_SELTAB to CT_SELTAB.
endform.                    "fill_seltab

*&---------------------------------------------------------------------*
*&      Form  callback_sdf_skc1a
*&---------------------------------------------------------------------*
form CALLBACK_SDF_SKC1A                                     "#EC CALLED
  using
    NODE    type LDBN-LDBNODE                               "#EC *
    WA      type SKC1A                                      "#EC *
    EVT     type C                                          "#EC *
    CHECK   type C.                                         "#EC *

  case EVT.
    when 'G'.
      append WA to GT_SKC1A_AUX.
  endcase.

endform.                    "callback_sdf_skc1a

*&---------------------------------------------------------------------*
*&      Form  callback_sdf_ska1
*&---------------------------------------------------------------------*
form CALLBACK_SDF_SKA1                                      "#EC CALLED
  using
    NODE    type LDBN-LDBNODE                               "#EC *
    WA      type SKA1                                       "#EC *
    EVT     type C                                          "#EC *
    CHECK   type C.                                         "#EC *

  data: LS_SKC1A type SKC1A.

  case EVT.
    when 'G'.
      refresh GT_SKC1A_AUX.
      clear GS_SKA1.
      GS_SKA1-SAKNR = WA-SAKNR.
      GS_SKA1-ERDAT = WA-ERDAT.
    when 'L'.
      loop at GT_SKC1A_AUX into LS_SKC1A.
        clear LS_SKC1A-GSBER. "Report doesn't use this level of detail
        collect LS_SKC1A into GTS_SKC1A.
      endloop.
  endcase.

endform.                    "callback_sdf_ska1

*&---------------------------------------------------------------------*
*&      Form  callback_sdf_skb1
*&---------------------------------------------------------------------*
form CALLBACK_SDF_SKB1                                      "#EC CALLED
  using
    NODE    type LDBN-LDBNODE                               "#EC *
    WA      type SKB1                                       "#EC *
    EVT     type C                                          "#EC *
    CHECK   type C.                                         "#EC *

  data: LS_SKA1 type TP_SKA1.

  case EVT.
    when 'G'. "Checking that account belongs to Company
      LS_SKA1-SAKNR = GS_SKA1-SAKNR.
      LS_SKA1-ERDAT = GS_SKA1-ERDAT.
      insert LS_SKA1 into table GT_SKA1. "Accounts in use in Company
  endcase.

endform.                    "callback_sdf_skb1

*&---------------------------------------------------------------------*
*&      Form  fill_fin_control
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form FILL_FIN_CONTROL.

  data: LV_CATEGORY_J100 type J_1BECD_REGISTER_CATEGORY,
        LV_CATEGORY_J150 type J_1BECD_REGISTER_CATEGORY,
        LV_CATEGORY_I155 type J_1BECD_REGISTER_CATEGORY,
        LV_CATEGORY_I350 type J_1BECD_REGISTER_CATEGORY,
        LV_CATEGORY_I355 type J_1BECD_REGISTER_CATEGORY.

  field-symbols: <TSLXX_IND> type ABAP_BOOL,
                 <TSLXX_DTI> type BUDAT,
                 <TSLXX_DTF> type BUDAT.

  data: LV_PER_INDEX type POPER, "period index
        LV_FIELDNAME type FIELDNAME.

  clear GS_FIN_CONTROL.
  do 12 times.
    LV_PER_INDEX = SY-INDEX.
    concatenate 'GS_FIN_CONTROL-TSL' LV_PER_INDEX+1 '_IND'
      into LV_FIELDNAME.
    assign (LV_FIELDNAME) to <TSLXX_IND>.
    concatenate 'GS_FIN_CONTROL-TSL' LV_PER_INDEX+1 '_DTI'
      into LV_FIELDNAME.
    assign (LV_FIELDNAME) to <TSLXX_DTI>.
    concatenate 'GS_FIN_CONTROL-TSL' LV_PER_INDEX+1 '_DTF'
      into LV_FIELDNAME.
    assign (LV_FIELDNAME) to <TSLXX_DTF>.
    perform CHECK_DATE_PERIOD using P_GJAHR
                                    GV_RLDNR_PERIV          "1410166
                                    LV_PER_INDEX
                           changing GS_FIN_CONTROL-DTINI
                                    GS_FIN_CONTROL-DTFIN
                                    <TSLXX_IND>
                                    <TSLXX_DTI>
                                    <TSLXX_DTF>
                                    GS_FIN_CONTROL-LAST_PERIOD.
  enddo.
  do 4 times.
    LV_PER_INDEX = 12 + SY-INDEX. "special periods
    concatenate 'GS_FIN_CONTROL-TSL' LV_PER_INDEX+1 '_IND'
      into LV_FIELDNAME.
    assign (LV_FIELDNAME) to <TSLXX_IND>.
    assign GS_FIN_CONTROL-TSL12_DTI to <TSLXX_DTI>. "last
    assign GS_FIN_CONTROL-TSL12_DTF to <TSLXX_DTF>. "last
    perform CHECK_DATE_PERIOD using P_GJAHR
                                    GV_RLDNR_PERIV          "1410166
                                    LV_PER_INDEX
                           changing GS_FIN_CONTROL-DTINI
                                    GS_FIN_CONTROL-DTFIN
                                    <TSLXX_IND>
                                    <TSLXX_DTI>
                                    <TSLXX_DTF>
                                    GS_FIN_CONTROL-LAST_PERIOD.
  enddo.

* TOTAL_ACC_IND_J + TOTAL_ACC_IND_I: Indicates that when selecting
* accounts, account balances should also be selected.
* TOTAL_ACC_IND_J alone: Indicates that preparation for J100
* and/or J150 is needed, so account balances are needed as well as
* aglutination code buffering.
  perform GET_CATEGORY using CONST_REG_J100
                    changing LV_CATEGORY_J100.
  perform GET_CATEGORY using CONST_REG_J150
                    changing LV_CATEGORY_J150.
  if LV_CATEGORY_J100 = CONST_NOTUSED and
     LV_CATEGORY_J150 = CONST_NOTUSED.
    GS_FIN_CONTROL-TOTAL_ACC_IND_J = ABAP_FALSE.
  else.
    GS_FIN_CONTROL-TOTAL_ACC_IND_J = ABAP_TRUE.
  endif.

  perform GET_CATEGORY using CONST_REG_I155
                    changing LV_CATEGORY_I155.
  if LV_CATEGORY_I155 = CONST_NOTUSED.
    GS_FIN_CONTROL-TOTAL_ACC_IND_I = ABAP_FALSE.
  else.
    GS_FIN_CONTROL-TOTAL_ACC_IND_I = ABAP_TRUE.
  endif.

* TOTAL_ACC_CLOS: Indicates that when selecting FI documents,
* buffering of closing FI documents should be done, in order to
* prevent further selections over the same data
* ... i355 deppends on i350
  perform GET_CATEGORY using CONST_REG_I350
                    changing LV_CATEGORY_I350.
  perform GET_CATEGORY using CONST_REG_I355
                    changing LV_CATEGORY_I355.
  if P_DOCTYP is initial. "no way to buffer closing info
    GS_FIN_CONTROL-TOTAL_ACC_CLOS = ABAP_FALSE.
  elseif ( LV_CATEGORY_I350 = CONST_NOTUSED "no need
           or LV_CATEGORY_I355 = CONST_NOTUSED )
         and LV_CATEGORY_J150 = CONST_NOTUSED.
    GS_FIN_CONTROL-TOTAL_ACC_CLOS = ABAP_FALSE.
  else.
    GS_FIN_CONTROL-TOTAL_ACC_CLOS = ABAP_TRUE.
  endif.

* Replace dates in case of Special Situation
  if not P_INDESP is initial.
    if not P_DTESPI is initial.
      GS_FIN_CONTROL-DTINI = P_DTESPI.
    endif.
    if not P_DTESPF is initial.
      GS_FIN_CONTROL-DTFIN = P_DTESPF.
    endif.
  endif.

endform.                    "fill_fin_control

*&---------------------------------------------------------------------*
*&      Form  check_date_period
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->IV_YEAR    text
*      -->IV_PERIV   text
*      -->IV_POPER   text
*      -->CV_DTINI   text
*      -->CV_DTFIN   text
*      -->CV_IND     text
*      -->CV_PERDTI  text
*      -->CV_PERDTF  text
*      -->CV_LAST    text
*----------------------------------------------------------------------*
form CHECK_DATE_PERIOD using IV_YEAR   type GJAHR
                             IV_PERIV  type PERIV
                             IV_POPER  type POPER
                    changing CV_DTINI  type BUDAT
                             CV_DTFIN  type BUDAT
                             CV_IND    type BOOLE_D
                             CV_PERDTI type BUDAT
                             CV_PERDTF type BUDAT
                             CV_LAST   type POPER.
  data: LV_DATE type SYDATUM.
  CV_IND = ABAP_FALSE.
  if IV_POPER in S_MONAT.
    CV_IND = ABAP_TRUE.
    if CV_LAST is initial or
    CV_LAST < IV_POPER.
      CV_LAST = IV_POPER. "the latest period found
    endif.
    call function 'FIRST_DAY_IN_PERIOD_GET'
      exporting
        I_GJAHR        = IV_YEAR
*       I_MONMIT       = 00
        I_PERIV        = IV_PERIV
        I_POPER        = IV_POPER
      importing
        E_DATE         = LV_DATE
      exceptions
        INPUT_FALSE    = 1
        T009_NOTFOUND  = 2
        T009B_NOTFOUND = 3
        others         = 4.
    if SY-SUBRC = 0.
      if CV_DTINI is initial or
      CV_DTINI > LV_DATE.
        CV_DTINI = LV_DATE. "the earliest date found
      endif.
      CV_PERDTI = LV_DATE.
    endif.
    call function 'LAST_DAY_IN_PERIOD_GET'
      exporting
        I_GJAHR        = IV_YEAR
*       I_MONMIT       = 00
        I_PERIV        = IV_PERIV
        I_POPER        = IV_POPER
      importing
        E_DATE         = LV_DATE
      exceptions
        INPUT_FALSE    = 1
        T009_NOTFOUND  = 2
        T009B_NOTFOUND = 3
        others         = 4.
    if SY-SUBRC = 0.
      if CV_DTFIN is initial or
      CV_DTFIN < LV_DATE.
        CV_DTFIN = LV_DATE. "the latest date found
      endif.
      CV_PERDTF = LV_DATE.
    endif.
  endif.
endform.                    "check_date_period

*&---------------------------------------------------------------------*
*&      Form  get_category
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->IV_REGISTER  text
*      -->CV_CATEGORY  text
*----------------------------------------------------------------------*
form GET_CATEGORY  using    IV_REGISTER type J_1BECD_REGISTER
                   changing CV_CATEGORY type J_1BECD_REGISTER_CATEGORY.

  read table GT_CONTROL with table key
     REGNO = IV_REGISTER
     into GS_CONTROL.
  if SY-SUBRC is initial.
    case P_INDESC.
      when 'G'.
        CV_CATEGORY = GS_CONTROL-TYPE_G.
        if P_VAR = 'X' and not GS_CONTROL-TYPE_G_VAR is initial.
          CV_CATEGORY = GS_CONTROL-TYPE_G_VAR.
        endif.
      when 'R'.
        CV_CATEGORY = GS_CONTROL-TYPE_R.
        if P_VAR = 'X' and not GS_CONTROL-TYPE_R_VAR is initial.
          CV_CATEGORY = GS_CONTROL-TYPE_R_VAR.
        endif.
      when 'A'.
        CV_CATEGORY = GS_CONTROL-TYPE_A.
        if P_VAR = 'X' and not GS_CONTROL-TYPE_A_VAR is initial.
          CV_CATEGORY = GS_CONTROL-TYPE_A_VAR.
        endif.
      when 'B'.
        CV_CATEGORY = GS_CONTROL-TYPE_B.
        if P_VAR = 'X' and not GS_CONTROL-TYPE_B_VAR is initial.
          CV_CATEGORY = GS_CONTROL-TYPE_B_VAR.
        endif.
      when 'Z'.
        CV_CATEGORY = GS_CONTROL-TYPE_Z.
        if P_VAR = 'X' and not GS_CONTROL-TYPE_Z_VAR is initial.
          CV_CATEGORY = GS_CONTROL-TYPE_Z_VAR.
        endif.
    endcase.
  endif.

endform.                    " GET_CATEGORY

*&---------------------------------------------------------------------*
*&      Form  fill_j100_vl_cta_ini
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_BUKRS    text
*      -->P_SAKNR    text
*      -->CS_J100    text
*----------------------------------------------------------------------*
form FILL_J100_VL_CTA_INI
  using
    P_BUKRS type BUKRS
    P_SAKNR type J_1BECD_ACCOUNT_CODE
  changing
    CS_J100 type J_1BECD_J100_3_S.

  field-symbols: <LFS_UMXXK> type SKC1A-UMSAV.              "2168678

  data: LV_UMXXK type FIELDNAME,
        LS_SKC1A type SKC1A.

  clear: CS_J100-VL_CTA_INI.

  read table GTS_SKC1A into LS_SKC1A
    with key MANDT = SY-MANDT
             BUKRS = P_BUKRS
             SAKNR = P_SAKNR.

  if SY-SUBRC = 0.

    if P_TRI is initial.    " Annual          " 2190937
      LV_UMXXK = 'LS_SKC1A-UMSAV'.                          " 2190937
    elseif S_MONAT-LOW > 9. " dec/nov/oct     " 2190937
      LV_UMXXK = 'LS_SKC1A-UM09K'.                          " 2190937
    elseif S_MONAT-LOW > 6. " sep/aug/jul     " 2190937
      LV_UMXXK = 'LS_SKC1A-UM06K'.                          " 2190937
    elseif S_MONAT-LOW > 3. " jun/may/apr     " 2190937
      LV_UMXXK = 'LS_SKC1A-UM03K'.                          " 2190937
    else.                   " mar/feb/jan     " 2190937
      LV_UMXXK = 'LS_SKC1A-UM01K'.                          " 2190937
    endif.                                                  " 2190937

    assign (LV_UMXXK) to <LFS_UMXXK>.
    if SY-SUBRC = 0.
      CS_J100-VL_CTA_INI = <LFS_UMXXK>.
    endif.

  endif.

endform.                    "fill_j100_vl_cta_ini

*&---------------------------------------------------------------------*
*&      Form  create_versions
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->GT_VERSIONS  text
*      -->REG          text
*      -->LAYOUT       text
*      -->STYPE        text
*----------------------------------------------------------------------*
form CREATE_VERSIONS tables GT_VERSIONS type TT_VERSIONS    "1909036
                     using  REG    type STRING              "1909036
                            LAYOUT type J_1BECD_COD_VER     "1909036
                            STYPE  type STRING.             "1909036

  data LS_VERSIONS like line of GT_VERSIONS.                "1909036

  clear LS_VERSIONS.                                        "1909036
  LS_VERSIONS-REGISTER = REG.                               "1909036
  LS_VERSIONS-LAYOUT   = LAYOUT.                            "1909036
  LS_VERSIONS-TYPE     = STYPE.                             "1909036
  append LS_VERSIONS to GT_VERSIONS.                        "1909036

endform.                                                    "1909036
*&---------------------------------------------------------------------*
*&      Form  PROCESS_RAW_REGISTER
*&---------------------------------------------------------------------*
*       text
form PROCESS_RAW_REGISTER using IV_BLOCK    type C
                                IS_RECORD   type ANY
                                IV_REGISTER type J_1BECD_REGISTER.

  data: LV_CATEGORY type J_1BECD_REGISTER_CATEGORY.

* Check applicable category of register
  perform GET_CATEGORY using IV_REGISTER
                       changing LV_CATEGORY.
  if LV_CATEGORY = CONST_NOTUSED or
     LV_CATEGORY is initial.
    return. "Do not process record
  endif.

* Check higher record for registers mandatory but depending on hierarchy
  if LV_CATEGORY = CONST_DEPEND_H and IS_RECORD is initial.
    return. "Do not process record
  endif.

* Call the register filling routine
  case IV_BLOCK.
    when 'I'.
* Block I
      case IV_REGISTER.
        when CONST_REG_I550.
          "  PERFORM PROCESS_REG_I550_I555.
      endcase.
    when 'J'.
* Block J
      case IV_REGISTER.
        when CONST_REG_J800.
          perform PROCESS_REG_J800.
        when CONST_REG_J801.
          perform PROCESS_REG_J801.
      endcase.
  endcase.

endform.                    "process_raw_register

form PROCESS_REG_J800.
  data LV_FILENAME type STRING.
  data LT_FILENAME type STRING_TABLE.

  if P_PCIN is initial and P_APIN is initial.
    return. "no file is required (J800 is facultative)
  endif.

  perform FILE_UPLOAD using P_XPCIN P_PCIN P_XAPIN P_APIN changing LV_FILENAME. "2416906
  if IT_FILE is not initial.
*--- One file only
    perform PROCESS_UPLOADED_FILE using CONST_REG_J800 P_TYPDOC P_DESRTF CONST_J800FIM. "2416906
  else.
*--- Not a file, but a folder (possible several files)
    perform DIRECTORY_LIST tables LT_FILENAME
                            using LV_FILENAME.
    loop at LT_FILENAME into LV_FILENAME.
      perform FILE_UPLOAD using P_XPCIN P_PCIN P_XAPIN P_APIN changing LV_FILENAME. "2416906
      perform PROCESS_UPLOADED_FILE using CONST_REG_J800 P_TYPDOC P_DESRTF CONST_J800FIM.
    endloop.
  endif.

endform.

form PROCESS_REG_J801.                                      "2416906
  data LV_FILENAME type STRING.                             "2416906
  data LT_FILENAME type STRING_TABLE.                       "2416906
                                                            "2416906
  if P_SPCIN is initial and P_SAPIN is initial.             "2416906
    return. "no file is required (J801 is facultative)            "2416906
  endif.                                                    "2416906
                                                            "2416906
  perform FILE_UPLOAD using P_SXPCIN P_SPCIN P_SXAPIN P_SAPIN changing LV_FILENAME. "2416906
  if IT_FILE is not initial.                                "2416906
*--- One file only                                                "2416906
    perform PROCESS_UPLOADED_FILE using CONST_REG_J801 P_STYPDO P_SDESRT CONST_J801FIM. "2416906
  else.                                                     "2416906
*--- Not a file, but a folder (possible several files)            "2416906
    perform DIRECTORY_LIST tables LT_FILENAME               "2416906
                            using LV_FILENAME.              "2416906
    loop at LT_FILENAME into LV_FILENAME.                   "2416906
      perform FILE_UPLOAD using P_SXPCIN P_SPCIN P_SXAPIN P_SAPIN changing LV_FILENAME. "2416906
      perform PROCESS_UPLOADED_FILE using CONST_REG_J801 P_STYPDO P_SDESRT CONST_J801FIM. "2416906
    endloop.                                                "2416906
  endif.                                                    "2416906
                                                            "2416906
endform.                    " process_reg_J801                    "2416906


form FILE_UPLOAD  using UV_XPCIN  type XFELD
                        UV_PCIN   type J_1BECD_ATTACHMENT
                        UV_XAPIN  type XFELD
                        UV_APIN   type J_1BECD_ATTACHMENT
                  changing IV_FILENAME type STRING.

  data LV_FILENAME_AS   type LOCALFILE.
  data LS_FILE          type TP_ROW_FILE.
  data LV_ISFILEOPEN    type ABAP_BOOL.
  data LV_CHECK_FILE    type ABAP_BOOL.
  constants LOGICAL_FILENAME type FILENAME-FILEINTERN
              value 'FI_SPED_ECD_INPUT_FILE'.

  refresh IT_FILE.

* ------- Upload from presentation server ---------------------
  if UV_XPCIN = 'X'.
    if IV_FILENAME is initial.
      IV_FILENAME = UV_PCIN. "use filename indicated in selection screen
    else.
      LV_CHECK_FILE = ABAP_TRUE.
    endif.
    call function 'GUI_VSS_UPLOAD'
      exporting
        FILENAME        = IV_FILENAME
      tables
        DATA_TAB        = IT_FILE
      exceptions
        FILE_OPEN_ERROR = 1
        FILE_READ_ERROR = 2
        BAD_DATA_FORMAT = 2
        NO_AUTHORITY    = 3
        ACCESS_DENIED   = 3
        others          = 4.
    if SY-SUBRC <> 0 and LV_CHECK_FILE = ABAP_TRUE.
      SY-MSGTY = 'E'.
      SY-MSGID = 'J_1BECD'.
      if SY-SUBRC = 1.
        SY-MSGNO = '022'.
      elseif SY-SUBRC = 2.
        SY-MSGNO = '023'.
      elseif SY-SUBRC = 3.
        SY-MSGNO = '024'.
      endif.
      SY-MSGV1 = IV_FILENAME.
      call function 'J_1BECD_LOG_ADD_MESSAGE'
        exporting
          IV_EXCNO = GV_EXCNO.
    endif.

* ------- Upload from application server ---------------------
  elseif UV_XAPIN = 'X'.
    if IV_FILENAME is initial.
      IV_FILENAME = UV_APIN. "use filename indicated in selection screen
    else.
      LV_CHECK_FILE = ABAP_TRUE.
    endif.
    LV_FILENAME_AS = IV_FILENAME.
    try.
        call function 'FILE_VALIDATE_NAME'
          exporting
            LOGICAL_FILENAME  = LOGICAL_FILENAME
          changing
            PHYSICAL_FILENAME = LV_FILENAME_AS
          exceptions
            others            = 1.

        if SY-SUBRC <> 0.
          message id SY-MSGID type SY-MSGTY number SY-MSGNO
            with SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
        else.
          open dataset LV_FILENAME_AS for input
                                      in text mode
                                      encoding default
                                      with windows linefeed.
          if SY-SUBRC <> 0.
            if LV_CHECK_FILE = ABAP_TRUE.
              SY-MSGTY = 'E'.
              SY-MSGID = 'J_1BECD'.
              SY-MSGNO = '022'.
              SY-MSGV1 = IV_FILENAME.
              call function 'J_1BECD_LOG_ADD_MESSAGE'
                exporting
                  IV_EXCNO = GV_EXCNO.
            endif.
          else.
            LV_ISFILEOPEN = ABAP_TRUE.
            do.
              clear LS_FILE.
              read dataset LV_FILENAME_AS into LS_FILE-CONTENT.
              if SY-SUBRC ne 0.
                exit.
              endif.
              append LS_FILE to IT_FILE.
            enddo.
            close dataset LV_FILENAME_AS.
          endif.
        endif.
      catch CX_SY_FILE_IO.
* Filename refers to directory name
        if LV_ISFILEOPEN = ABAP_TRUE.
          close dataset LV_FILENAME_AS.
        endif.
    endtry.
  endif.
endform.                    " file_upload

*&---------------------------------------------------------------------*
*&      Form  process_uploaded_file
*&---------------------------------------------------------------------*
form PROCESS_UPLOADED_FILE using  US_REG      type J_1BECD_REGISTER "2416906
                                  US_TIPO_DOC type J_1BECD_TIPO_DOC "2416906
                                  US_DESC_RTF type J_1BECD_DESC_RTF "2416906
                                  US_FIM      type CHAR7.   "2416906
  data LS_RESULT type TP_RESULT_LINE.
  data LS_FILE   type TP_ROW_FILE.

  clear LS_RESULT.
  if P_LAYOUT < '005'.                                      "2416906
    concatenate CONST_SEPARATOR
                US_REG                                      "2416906
                CONST_SEPARATOR
           into LS_RESULT.
  else.                                                     "2416906
    concatenate CONST_SEPARATOR                             "2416906
                US_REG                                      "2416906
                CONST_SEPARATOR                             "2416906
                US_TIPO_DOC                                 "2416906
                CONST_SEPARATOR                             "2416906
                US_DESC_RTF                                 "2416906
                CONST_SEPARATOR                             "2416906
                CONST_SEPARATOR                             "2416906
           into LS_RESULT.                                  "2416906
  endif.                                                    "2416906

  loop at IT_FILE into LS_FILE.
    concatenate LS_RESULT LS_FILE-CONTENT into LS_RESULT.
  endloop.

  concatenate LS_RESULT
              CONST_SEPARATOR
              US_FIM                                        "2416906
              CONST_SEPARATOR
         into LS_RESULT.
  perform APPEND_RAW_LINE using LS_RESULT
                                US_REG.                     "2416906
  free IT_FILE.
endform.                    "process_uploaded_file

form APPEND_RAW_LINE using IS_RESULT    type TP_RESULT_LINE
                           IV_REGISTER  type J_1BECD_REGISTER.

  if GV_APPSV = ABAP_TRUE.
    transfer IS_RESULT to P_APOUT.
    if SY-SUBRC <> 0.
      message E027 with P_APOUT.
    endif.
  elseif GV_LCLSV = ABAP_TRUE.
    append IS_RESULT to GT_RESULT.
  else. "p_test = 'X'.
    write / IS_RESULT.
  endif.

* Increase counter (which counts entries per register)
  perform INCREASE_COUNTER using IV_REGISTER 1.

endform.                    "append_raw_line
*&----------------------------------------------------
*&      Form  INCREASE_COUNTER
*&---------------------------------------------------------------------*
form INCREASE_COUNTER  using IV_REGISTER type J_1BECD_REGISTER
                             IV_NUMBER   type I.

  case IV_REGISTER.
*   Registers of block 0
    when '0000'.
      GS_COUNTER-REG_0000 = GS_COUNTER-REG_0000 + IV_NUMBER.
    when '0001'.
      GS_COUNTER-REG_0001 = GS_COUNTER-REG_0001 + IV_NUMBER.
    when '0007'.
      GS_COUNTER-REG_0007 = GS_COUNTER-REG_0007 + IV_NUMBER.
    when '0020'.
      GS_COUNTER-REG_0020 = GS_COUNTER-REG_0020 + IV_NUMBER.
    when '0035'.                                            "2154499
      GS_COUNTER-REG_0035 = GS_COUNTER-REG_0035 + IV_NUMBER. "2154499
    when '0150'.
      GS_COUNTER-REG_0150 = GS_COUNTER-REG_0150 + IV_NUMBER.
    when '0180'.
      GS_COUNTER-REG_0180 = GS_COUNTER-REG_0180 + IV_NUMBER.
    when '0990'.
      GS_COUNTER-REG_0990 = GS_COUNTER-REG_0990 + IV_NUMBER.
*   Registers of block I
    when 'I001'.
      GS_COUNTER-REG_I001 = GS_COUNTER-REG_I001 + IV_NUMBER.
    when 'I010'.
      GS_COUNTER-REG_I010 = GS_COUNTER-REG_I010 + IV_NUMBER.
    when 'I012'.
      GS_COUNTER-REG_I012 = GS_COUNTER-REG_I012 + IV_NUMBER.
    when 'I015'.
      GS_COUNTER-REG_I015 = GS_COUNTER-REG_I015 + IV_NUMBER.
    when 'I020'.
      GS_COUNTER-REG_I020 = GS_COUNTER-REG_I020 + IV_NUMBER.
    when 'I030'.
      GS_COUNTER-REG_I030 = GS_COUNTER-REG_I030 + IV_NUMBER.
    when 'I050'.
      GS_COUNTER-REG_I050 = GS_COUNTER-REG_I050 + IV_NUMBER.
    when 'I051'.
      GS_COUNTER-REG_I051 = GS_COUNTER-REG_I051 + IV_NUMBER.
    when 'I052'.
      GS_COUNTER-REG_I052 = GS_COUNTER-REG_I052 + IV_NUMBER.
    when 'I053'.                                            "2154499
      GS_COUNTER-REG_I053 = GS_COUNTER-REG_I053 + IV_NUMBER. "2154499
    when 'I075'.
      GS_COUNTER-REG_I075 = GS_COUNTER-REG_I075 + IV_NUMBER.
    when 'I100'.
      GS_COUNTER-REG_I100 = GS_COUNTER-REG_I100 + IV_NUMBER.
    when 'I150'.
      GS_COUNTER-REG_I150 = GS_COUNTER-REG_I150 + IV_NUMBER.
    when 'I151'.                                            "1378742
      GS_COUNTER-REG_I151 = GS_COUNTER-REG_I151 + IV_NUMBER. "1378742
    when 'I155'.
      GS_COUNTER-REG_I155 = GS_COUNTER-REG_I155 + IV_NUMBER.
    when 'I157'.                                            "1909036
      GS_COUNTER-REG_I157 = GS_COUNTER-REG_I157 + IV_NUMBER. "1909036
    when 'I200'.
      GS_COUNTER-REG_I200 = GS_COUNTER-REG_I200 + IV_NUMBER.
    when 'I250'.
      GS_COUNTER-REG_I250 = GS_COUNTER-REG_I250 + IV_NUMBER.
    when 'I300'.
      GS_COUNTER-REG_I300 = GS_COUNTER-REG_I300 + IV_NUMBER.
    when 'I310'.
      GS_COUNTER-REG_I310 = GS_COUNTER-REG_I310 + IV_NUMBER.
    when 'I350'.
      GS_COUNTER-REG_I350 = GS_COUNTER-REG_I350 + IV_NUMBER.
    when 'I355'.
      GS_COUNTER-REG_I355 = GS_COUNTER-REG_I355 + IV_NUMBER.
    when 'I500'.
      GS_COUNTER-REG_I500 = GS_COUNTER-REG_I500 + IV_NUMBER.
    when 'I510'.
      GS_COUNTER-REG_I510 = GS_COUNTER-REG_I510 + IV_NUMBER.
    when 'I550'.
      GS_COUNTER-REG_I550 = GS_COUNTER-REG_I550 + IV_NUMBER.
    when 'I555'.
      GS_COUNTER-REG_I555 = GS_COUNTER-REG_I555 + IV_NUMBER.
    when 'I990'.
      GS_COUNTER-REG_I990 = GS_COUNTER-REG_I990 + IV_NUMBER.
*   Registers of block J
    when 'J001'.
      GS_COUNTER-REG_J001 = GS_COUNTER-REG_J001 + IV_NUMBER.
    when 'J005'.
      GS_COUNTER-REG_J005 = GS_COUNTER-REG_J005 + IV_NUMBER.
    when 'J100'.
      GS_COUNTER-REG_J100 = GS_COUNTER-REG_J100 + IV_NUMBER.
    when 'J150'.
      GS_COUNTER-REG_J150 = GS_COUNTER-REG_J150 + IV_NUMBER.
    when 'J200'.                                            "1909036
      GS_COUNTER-REG_J200 = GS_COUNTER-REG_J200 + IV_NUMBER. "1909036
    when 'J210'.                                            "1909036
      GS_COUNTER-REG_J210 = GS_COUNTER-REG_J210 + IV_NUMBER. "1909036
    when 'J215'.                                            "1909036
      GS_COUNTER-REG_J215 = GS_COUNTER-REG_J215 + IV_NUMBER. "1909036
*   WHEN 'J310'.                                             "1909036 "1988543
*     gs_counter-reg_j310 = gs_counter-reg_j310 + iv_number. "1909036 "1988543
*   WHEN 'J410'.                                             "1909036 "1988543
*     gs_counter-reg_j410 = gs_counter-reg_j410 + iv_number. "1909036 "1988543
    when 'J800'.
      GS_COUNTER-REG_J800 = GS_COUNTER-REG_J800 + IV_NUMBER.
    when 'J801'.                                            "2416906
      GS_COUNTER-REG_J801 = GS_COUNTER-REG_J801 + IV_NUMBER. "2416906
    when 'J900'.
      GS_COUNTER-REG_J900 = GS_COUNTER-REG_J900 + IV_NUMBER.
    when 'J930'.
      GS_COUNTER-REG_J930 = GS_COUNTER-REG_J930 + IV_NUMBER.
    when 'J935'.                                            "2154499
      GS_COUNTER-REG_J935 = GS_COUNTER-REG_J935 + IV_NUMBER. "2154499
    when 'J990'.
      GS_COUNTER-REG_J990 = GS_COUNTER-REG_J990 + IV_NUMBER.
*   Registers of block K                                          "2416906
    when 'K001'.                                            "2416906
      GS_COUNTER-REG_K001 = GS_COUNTER-REG_K001 + IV_NUMBER. "2416906
    when 'K030'.                                            "2416906
      GS_COUNTER-REG_K030 = GS_COUNTER-REG_K030 + IV_NUMBER. "2416906
    when 'K100'.                                            "2416906
      GS_COUNTER-REG_K100 = GS_COUNTER-REG_K100 + IV_NUMBER. "2416906
    when 'K110'.                                            "2416906
      GS_COUNTER-REG_K110 = GS_COUNTER-REG_K110 + IV_NUMBER. "2416906
    when 'K115'.                                            "2416906
      GS_COUNTER-REG_K115 = GS_COUNTER-REG_K115 + IV_NUMBER. "2416906
    when 'K200'.                                            "2416906
      GS_COUNTER-REG_K200 = GS_COUNTER-REG_K200 + IV_NUMBER. "2416906
    when 'K210'.                                            "2416906
      GS_COUNTER-REG_K210 = GS_COUNTER-REG_K210 + IV_NUMBER. "2416906
    when 'K300'.                                            "2416906
      GS_COUNTER-REG_K300 = GS_COUNTER-REG_K300 + IV_NUMBER. "2416906
    when 'K310'.                                            "2416906
      GS_COUNTER-REG_K310 = GS_COUNTER-REG_K310 + IV_NUMBER. "2416906
    when 'K315'.                                            "2416906
      GS_COUNTER-REG_K315 = GS_COUNTER-REG_K315 + IV_NUMBER. "2416906
    when 'K990'.                                            "2416906
      GS_COUNTER-REG_K990 = GS_COUNTER-REG_K990 + IV_NUMBER. "2416906
*   Registers of block 9
    when '9001'.
      GS_COUNTER-REG_9001 = GS_COUNTER-REG_9001 + IV_NUMBER.
    when '9900'.
      GS_COUNTER-REG_9900 = GS_COUNTER-REG_9900 + IV_NUMBER.
    when '9990'.
      GS_COUNTER-REG_9990 = GS_COUNTER-REG_9990 + IV_NUMBER.
    when '9999'.
      GS_COUNTER-REG_9999 = GS_COUNTER-REG_9999 + IV_NUMBER.
  endcase.

endform.                    " INCREASE_COUNTER

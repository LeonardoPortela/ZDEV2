"Name: \PR:SAPLIQS0\FO:SELECT_PAPER_F55\SE:BEGIN\EI
ENHANCEMENT 0 ZFTPM_NOTA_F02.

TYPES: BEGIN OF TY_AFVC.
        INCLUDE TYPE AFVC.
TYPES AUFNR TYPE AFKO-AUFNR.
TYPES END OF TY_AFVC.

DATA: V_FMNAME  TYPE RS38L_FNAM,
        V_CONTROL TYPE SSFCTRLOP,
        V_OUTPUT  TYPE SSFCOMPOP.

DATA: LS_ZETPM_IMP_ORDEM TYPE  TABLE OF ZETPM_IMP_ORDEM.
DATA: LT_ZETPM_IMP_ORDEM TYPE  ZETPM_IMP_ORDEM_T.
DATA: LW_ZETPM_IMP_ORDEM TYPE ZETPM_IMP_ORDEM.
DATA  LS_AFVC TYPE TABLE OF TY_AFVC.
DATA: ls_t001k  TYPE  t001k,
        s_swerk   TYPE swerk,
        s_name    TYPE tdsfname,
        s_name1   TYPE tdsfname.


 FREE: LT_ZETPM_IMP_ORDEM, LS_AFVC.
 CLEAR: LS_ZETPM_IMP_ORDEM, LW_ZETPM_IMP_ORDEM.

DATA: zuretcd, zende_popup.
  DATA: zpm_doc_typ LIKE t390_o-pm_doc_typ.
  DATA: zprint_view LIKE itcpp-tdpreview." flag for print view requested.
  DATA: zo_doc_typ2 LIKE t390_o-pm_doc_typ,
        zo_appl2    LIKE iprt_wa-pm_appl.
  DATA: zlcommit VALUE ' '.
  DATA: zl_return LIKE sy-subrc VALUE 0.
  DATA zlb_iqs0_pdf_paper_selection TYPE REF TO badi_iqs0_pdf_paper_selection.
  DATA zlv_pdf_paper_selection TYPE xflag VALUE space.
  DATA zlv_printwosave TYPE qnprintwosave.
  DATA zlt_documents TYPE eams_document_list_t.

  DATA: zlt_return      TYPE bapirettab,
        zlv_abbruch(1)  TYPE c,
        zlv_answer_no   TYPE abap_bool,
        zlv_selected    TYPE boolean.

  FIELD-SYMBOLS <zls_iworkpaper> LIKE LINE OF iworkpaper.

  CONSTANTS zid_iprt_document(16) VALUE 'ID_IPRT_DOCUMENT'.


  ind_liste_druck = space.             " not in IW28 list print
  zpm_doc_typ = viqmel-qmart.           " save Meldungsart for PRINT
*-> order related to notification
  PERFORM fill_buffer_from_wa_f01.

*  PERFORM BUCH_F01 USING LCOMMIT L_RETURN.

  PERFORM order_print_check USING viqmel-aufnr
                                  zo_doc_typ2
                                  zo_appl2
                                  retcd.
  CHECK retcd = 0.                     " printing allowed

  IF cl_eams_switch_check=>eam_sfws_eams( ) IS NOT INITIAL.
    IMPORT zidocuments = zlt_documents FROM MEMORY ID zid_iprt_document.
  ENDIF.

* display popup for paper selection
  CALL FUNCTION 'PM_VALID_WORKPAPERS'
    EXPORTING
      pm_doc_typ                = zpm_doc_typ
      ingrp                     = viqmel-ingrp
      iwerk                     = viqmel-iwerk
      pm_appl                   = yn
      pm_appl_2                 = zo_appl2
      pm_doc_typ_2              = zo_doc_typ2
      uname                     = sy-uname
      proc_dark                 = p_dark
      aufnr                     = viqmel-aufnr
      qmnum                     = viqmel-qmnum
      qmherkz                   = viqmel-herkz
      qmaktyp                   = t365-aktyp
    IMPORTING
      rc                        = retcd
      print_view                = zprint_view
      device                    = device
      printwosave               = zlv_printwosave
    TABLES
      iworkpaper                = iworkpaper
      idocuments                = zlt_documents
    EXCEPTIONS
      invalid_document_type     = 01
      invalid_generic_parameter = 02
      invalid_pm_application    = 03.

CHECK iworkpaper IS NOT INITIAL.

LOOP AT iworkpaper WHERE selected EQ 'X'.
CASE iworkpaper-workpaper.
  WHEN '1500'.
  INCLUDE ZFTPM_NOTA_F01.

  WHEN 'Z001'.
   SELECT *
  FROM VIAUFKST
  INTO CORRESPONDING FIELDS OF TABLE LS_ZETPM_IMP_ORDEM
    WHERE AUFNR EQ VIQMEL-AUFNR.

  CHECK LS_ZETPM_IMP_ORDEM IS NOT INITIAL.

  LOOP AT LS_ZETPM_IMP_ORDEM ASSIGNING FIELD-SYMBOL(<LS_ORDEM>).

    IF <LS_ORDEM>-EQUNR IS NOT INITIAL.
      SELECT SINGLE *
      FROM EQKT
      INTO @DATA(L_EQKT)
        WHERE EQUNR EQ @<LS_ORDEM>-EQUNR.

      <LS_ORDEM>-EQKTX = L_EQKT-EQKTX.
    ENDIF.

    <LS_ORDEM>-SDATE = SY-DATUM.

    SELECT SINGLE *
    FROM AUFK
    INTO @DATA(_AUFK)
      WHERE AUFNR EQ @CAUFVD-AUFNR.

    <LS_ORDEM>-ARBPL = _AUFK-VAPLZ.

    SELECT SINGLE *
    FROM V_AUART
    INTO @DATA(_V_AUART)
      WHERE AUART EQ @<LS_ORDEM>-AUART
       AND  SPRAS EQ 'P'.

    <LS_ORDEM>-TXT = _V_AUART-TXT.

    SELECT SINGLE *
    FROM T353I_T
    INTO @DATA(_T353I_T)
      WHERE ILART EQ @<LS_ORDEM>-ILART
      AND   SPRAS EQ 'P'.

    <LS_ORDEM>-ILATX = _T353I_T-ILATX.

    SELECT SINGLE *
    FROM CSKT
    INTO @DATA(L_CSKT)
      WHERE KOSTL EQ @<LS_ORDEM>-KOSTL.

    <LS_ORDEM>-TXT_KOSTL = L_CSKT-KTEXT.

    SELECT SINGLE *
    FROM IFLO
    INTO @DATA(L_IFLO)
      WHERE TPLNR EQ @<LS_ORDEM>-TPLNR.

    <LS_ORDEM>-PLTXT = L_IFLO-PLTXT.

    SELECT SINGLE *
    FROM T356_T
    INTO @DATA(_T356_T)
      WHERE PRIOK EQ @<LS_ORDEM>-PRIOK
        AND SPRAS EQ 'P'.
    <LS_ORDEM>-PRIOKX = _T356_T-PRIOKX.

    SELECT SINGLE *
    FROM T001W
    INTO @DATA(L_T001W)
    WHERE WERKS EQ @<LS_ORDEM>-IWERK.
    <LS_ORDEM>-NAME1 = L_T001W-NAME1.

    IF <LS_ORDEM>-GLTRP IS NOT INITIAL AND <LS_ORDEM>-GSTRP IS NOT INITIAL.
      <LS_ORDEM>-TMP_EXEC = <LS_ORDEM>-GLTRP - <LS_ORDEM>-GSTRP.
    ENDIF.

    CLEAR: s_swerk.
    s_swerk = <LS_ORDEM>-IWERK.

  ENDLOOP.

  SELECT *
  FROM AFVC AS A
  INNER JOIN AFKO AS B ON B~AUFPL EQ A~AUFPL
  INTO CORRESPONDING FIELDS OF TABLE LS_AFVC
   WHERE B~AUFNR EQ VIQMEL-AUFNR
     AND A~PHFLG NE ABAP_TRUE.
*
  CHECK LS_AFVC IS NOT INITIAL.

  LOOP AT LS_ZETPM_IMP_ORDEM INTO DATA(L_ORDEM).
    MOVE-CORRESPONDING  L_ORDEM TO LW_ZETPM_IMP_ORDEM.
    LOOP AT LS_AFVC INTO DATA(LW_AFVC) WHERE AUFNR EQ CAUFVD-AUFNR.
      LW_ZETPM_IMP_ORDEM-VORNR = LW_AFVC-VORNR.
      LW_ZETPM_IMP_ORDEM-ARBID = LW_AFVC-ARBID.
      LW_ZETPM_IMP_ORDEM-LTXA1 = LW_AFVC-LTXA1.

      SELECT SINGLE *
      FROM CRHD
      INTO @DATA(L_CRHD)
        WHERE OBJID EQ @LW_ZETPM_IMP_ORDEM-ARBID.

      LW_ZETPM_IMP_ORDEM-TXT_C_TRAB = L_CRHD-ARBPL.

      APPEND LW_ZETPM_IMP_ORDEM TO  LT_ZETPM_IMP_ORDEM.
    ENDLOOP.
  ENDLOOP.
*&===========================================

  "Verificar se ordem pertence a empresa TGG.
  CLEAR: ls_t001k.
  CALL FUNCTION 'AIP01_PLANT_DETERMINE'
    EXPORTING
      i_werks  = s_swerk
    IMPORTING
      es_t001k = ls_t001k
    EXCEPTIONS
      OTHERS   = 1.


  CLEAR: I_DATA.
  SELECT SINGLE *
    FROM setleaf
    INTO i_data
      WHERE setname EQ 'MAGI_PM_IW32'
        AND valfrom EQ ls_t001k-bukrs.

  CLEAR: s_name, s_name1.
  IF i_data IS INITIAL.
    s_name = 'ZPMF0003'.
  ELSE.
    s_name = 'ZPMF0009'.
  ENDIF.

  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
*     FORMNAME           = 'ZFTPM_SF_001'
*     FORMNAME           = 'ZPMF0002'
      FORMNAME           = s_name
    IMPORTING
      FM_NAME            = V_FMNAME
    EXCEPTIONS
      NO_FORM            = 1
      NO_FUNCTION_MODULE = 2
      OTHERS             = 3.

  IF SY-SUBRC = 0.

    V_CONTROL-PREVIEW     = ABAP_TRUE.
    V_CONTROL-NO_DIALOG   = ABAP_TRUE.
    V_OUTPUT-TDDEST       = 'LOCL'.
    V_OUTPUT-TDNOPRINT    = ABAP_TRUE.

    CALL FUNCTION V_FMNAME
      EXPORTING
*        CONTROL_PARAMETERS = V_CONTROL
*        OUTPUT_OPTIONS     = V_OUTPUT
*       CABECALHO          = WA_CABEC
        GS_ORDEM           = LW_ZETPM_IMP_ORDEM
*        USER_SETTINGS      = 'X'
      TABLES
        GT_ZETPM_IMP_ORDEM = LT_ZETPM_IMP_ORDEM
*       OPER               = IT_OPER
*       MATER              = IT_MATER
*       TEXT               = IT_TEXT
      EXCEPTIONS
        FORMATTING_ERROR   = 1
        INTERNAL_ERROR     = 2
        SEND_ERROR         = 3
        USER_CANCELED      = 4
        OTHERS             = 5.
  ENDIF.
  WHEN OTHERS.
ENDCASE.
ENDLOOP.
EXIT.


ENDENHANCEMENT.

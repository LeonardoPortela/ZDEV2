*&---------------------------------------------------------------------*
*&  Include           YHBRCVT03
*&---------------------------------------------------------------------*
*Last changes: macro call RP-SET-NAME-FORMAT replaced by function call
*RP_SET_NAME_FORMAT.
*Last modified: 01.11.1999
*Last modified by: Mario Sancho Graca

*{ Selection Screen
*SELECTION-SCREEN BEGIN OF BLOCK FRM1 WITH FRAME TITLE TEXT-001.
*SELECTION-SCREEN BEGIN OF LINE.
*SELECTION-SCREEN COMMENT 1(31) TEXT-003 for field list.
*PARAMETERS: LIST RADIOBUTTON GROUP MOD.
*SELECTION-SCREEN END OF LINE.
*SELECTION-SCREEN BEGIN OF LINE.
*SELECTION-SCREEN COMMENT 1(31) TEXT-002 for field tree.
*PARAMETERS: TREE RADIOBUTTON GROUP MOD.
*SELECTION-SCREEN END OF LINE.
*SELECTION-SCREEN END   OF BLOCK FRM1.

TABLES: sscrfields.

* Asmitidos no mês
SELECTION-SCREEN BEGIN OF BLOCK frm4 WITH FRAME TITLE TEXT-040.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 1(31) TEXT-041 FOR FIELD mes.
    PARAMETERS: mes RADIOBUTTON GROUP mo1.
  SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 1(31) TEXT-042 FOR FIELD adm.
    PARAMETERS: adm RADIOBUTTON GROUP mo1.
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END   OF BLOCK frm4.


"selection-screen begin of block frm2 with frame title text-035.
"*select-options: absences for p2001-subty.
"select-options: absences for hbrxxxx0-subty.
"selection-screen end   of block frm2.


SELECTION-SCREEN BEGIN OF BLOCK frm3 WITH FRAME TITLE TEXT-008.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 1(45) TEXT-009 FOR FIELD bti_15.
    PARAMETERS: bti_15 LIKE rpcalcx0-tst_on VALUE CHECK DEFAULT ' '.
    "PARAMETERS: BTI_15 LIKE hbrxxxx0-updat VALUE CHECK DEFAULT ' '.
  SELECTION-SCREEN END OF LINE.
  PARAMETERS: batname TYPE hbrxxxx0-mapn DEFAULT sy-uname.
SELECTION-SCREEN END   OF BLOCK frm3.
*}

SELECTION-SCREEN BEGIN OF BLOCK frm5 WITH FRAME TITLE TEXT-fm5.

  PARAMETERS  pa_path   TYPE localfile.

SELECTION-SCREEN END OF BLOCK frm5.

*** BUG - 103946 - CBRAND - 103946 Inicio
SELECTION-SCREEN BEGIN OF BLOCK frm6 WITH FRAME.
  SELECTION-SCREEN PUSHBUTTON 01(38) p_but1 USER-COMMAND info.
SELECTION-SCREEN END OF BLOCK frm6.
*** BUG - 103946 - CBRAND - Fim
AT SELECTION-SCREEN ON VALUE-REQUEST FOR pa_path.
  PERFORM zf_get_path CHANGING pa_path.

INITIALIZATION.

  CONCATENATE icon_create_text
 'Cadastro Divisão Vale Alimentação' INTO p_but1.

AT SELECTION-SCREEN OUTPUT.

  DATA l_bukrs TYPE bukrs.
  DATA l_begda TYPE begda.
  DATA l_endda TYPE endda.

  PERFORM get_tvarvc_bukrs CHANGING l_bukrs.

  pnpbukrs-low = l_bukrs.
  pnpbukrs-sign = 'I'.
  pnpbukrs-option = 'EQ'.
  COLLECT pnpbukrs.

*** BUG - 103946 - CBRAND - 103946 Inicio
*  l_begda = |{ sy-datum(6) }01|.
*
*  CALL FUNCTION 'HR_HCP_GET_LAST_DAY_OF_MONTH'
*    EXPORTING
*      im_date              = l_begda
*    IMPORTING
*      ex_last_day_of_month = l_begda.
*
*  l_begda = l_begda + 1.
*
*  CALL FUNCTION 'HR_HCP_GET_LAST_DAY_OF_MONTH'
*    EXPORTING
*      im_date              = l_begda
*    IMPORTING
*      ex_last_day_of_month = l_endda.
*
*  pnpbegda = l_begda.
*
*  pnpendda = l_endda.
*** BUG - 103946 - CBRAND - 103946 Fim

AT SELECTION-SCREEN.
  IF sscrfields-ucomm = 'INFO'.
    CALL TRANSACTION 'ZHCM_BN0024'.
  ENDIF.


*{ Main Program
START-OF-SELECTION.

  DATA l_bukrs TYPE bukrs.
  DATA absences TYPE RANGE OF subty.

  PERFORM get_tvarvc_bukrs CHANGING l_bukrs.

**** BUG - 109183 - CBRAND - Inicio
  SELECT *
  FROM tvarvc
  INTO @DATA(wtvarvc)
  WHERE name EQ 'ZHRBN_VT_AUSENCIAS'.

    APPEND VALUE #( sign = wtvarvc-sign option = wtvarvc-opti low = wtvarvc-low high = wtvarvc-high  ) TO absences.

  ENDSELECT.
**** BUG - 109183 - CBRAND - Fim

* Array fetch
  PERFORM progress_indicator USING 'Inicialização'(004).
  PERFORM init.

  DATA chk_dtini TYPE datum.
  DATA chk_dtfim TYPE datum.


  IF mes EQ 'X'.
    IF pn-begda+6(02) NE '01'.
      MESSAGE 'O periodo deve ser mensal!' TYPE 'I' DISPLAY LIKE 'E'.
      EXIT.
    ELSEIF pn-begda+4(02) NE pn-endda+4(02).
      MESSAGE 'O periodo deve ser mensal!' TYPE 'I' DISPLAY LIKE 'E'.
      EXIT.
    ELSE.
      pn-endda = pn-endda + 1.
      IF pn-begda+4(02) EQ pn-endda+4(02).
        MESSAGE 'O periodo deve ser mensal!' TYPE 'I' DISPLAY LIKE 'E'.
        EXIT.
      ELSE.
        pn-endda = pn-endda - 1.
      ENDIF.

    ENDIF.

  ENDIF.



  programme_name = sy-repid.
  CALL FUNCTION 'RP_SET_NAME_FORMAT'
    EXPORTING
      repid  = programme_name
    IMPORTING
      format = employee_name_format.

*  vt_program = sy-repid.          "for 'REUSE_ALV_LIST_DISPLAY
  CLEAR total. REFRESH total.

GET pernr.

  CLEAR l_hire_date.
  CLEAR l_fire_date.
  CLEAR l_afastado.

  LOOP AT p0001 WHERE begda LE pn-endda AND endda GE pn-begda.
  ENDLOOP.

  LOOP AT p0002 WHERE begda LE pn-endda AND endda GE pn-begda..
  ENDLOOP.

  LOOP AT p0000 WHERE begda LE pn-endda AND endda GE pn-begda..
  ENDLOOP.

  LOOP AT p0032 WHERE begda LE pn-endda AND endda GE pn-begda..
  ENDLOOP.

**** BUG - 109183 - CBRAND - Inicio
**  LOOP AT p2001 WHERE endda EQ '31129999'.
**    l_afastado = 'X'.
**  ENDLOOP.
**** BUG - 109183 - CBRAND - fim


  PERFORM verify_hire_fire USING pernr-pernr
                                 pn-begda
                                 pn-endda
                      CHANGING l_hire_date
                               l_fire_date.

  IF adm EQ 'X'.

    IF l_hire_date LT pn-begda OR l_hire_date GT pn-endda.
      REJECT.
    ENDIF.


    IF p0001-bukrs NE l_bukrs.
      REJECT.
    ENDIF.

    IF p0000-massn NE '02' AND p0000-massn NE '20' AND p0000-massn NE 'ZL'.
      REJECT.
    ENDIF.

  ELSE.
    IF l_hire_date GE pn-begda AND l_hire_date LE pn-endda.
      REJECT.
    ENDIF.
  ENDIF.


  IF ( NOT l_fire_date IS INITIAL AND l_fire_date LT pn-begda ).
    REJECT.
  ENDIF.



  ADD 1 TO total-sele.
  PERFORM progress_indicator USING 'Seleção de dados'(005).
  PERFORM collect_step_1.
  IF NOT w_p0377_valid IS INITIAL.
    PERFORM collect_step_2.
  ENDIF.

  ADD 1 TO total-proc.

END-OF-SELECTION.
  PERFORM update_infotype15.

  IF bti_15 EQ 'X'.
*** Preparação dos layouts
    PERFORM zf_fill_file.
    PERFORM zf_download_file USING pa_path.

  ENDIF.
*** Download


  "  CASE P_MODE.
  "    WHEN '1'.
  "      PERFORM REICON USING 'ICON_DATE'.
  "      PERFORM FILL_TREE_WA USING 1 'Vale Transporte'(006) 30 1
  "                                   W_ICON    4
  "                                   PN-BEGDA  10
  "                                   '-'       1
  "                                   PN-ENDDA  10.
  "      PERFORM FILL_TREE_PERSON.
  "    WHEN '2'.
  PERFORM create_fieldcat.
  PERFORM modify_fieldcat.
  PERFORM fill_alv_tab.
  "  ENDCASE.

  PERFORM progress_indicator USING 'Preparação da lista de saída'(007).
  "  CASE P_MODE.
  "    WHEN '1'.
  "      PERFORM DISPLAY_TREE.
  "    WHEN '2'.
  PERFORM display_list.
  "  ENDCASE.

  NEW-PAGE.
  PERFORM print_error_list.

  rejected = total-sele - total-proc.
  PERFORM catt_message(hbrcatt0) USING total-sele rejected.

*} End of Main Program

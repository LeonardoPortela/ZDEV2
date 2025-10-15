*&---------------------------------------------------------------------*
*& Report  ZIM05
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  zim05.
TABLES: csks, zim01_sol_ap_inv,
*leoobsi
        zim02_sol_ap_ctl.
*leoobsf
DATA: w_per  TYPE usr21-persnumber,
      wg_bloqueio TYPE zim02_sol_ap_ctl-blq_exerc.
*      W_NAME TYPE ADRP-NAME_TEXT.

INCLUDE <icons>.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS: so_bukrs     FOR zim01_sol_ap_inv-bukrs,
                so_gsber     FOR zim01_sol_ap_inv-gsber,
                so_kostl     FOR csks-kostl MATCHCODE OBJECT kost,
                so_fase      FOR zim01_sol_ap_inv-fase ,
                so_ano       FOR zim01_sol_ap_inv-ano DEFAULT sy-datum(4),
                so_posnr     for zim01_sol_ap_inv-posnr.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b0 WITH FRAME TITLE text-000.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(25) FOR FIELD s_safra.
SELECTION-SCREEN POSITION 30.
SELECT-OPTIONS:
                s_safra     FOR zim01_sol_ap_inv-safra  NO INTERVALS  NO-EXTENSION.
*                SELECTION-SCREEN POSITION 43.
SELECTION-SCREEN COMMENT 43(12) FOR FIELD s_safra2.
SELECTION-SCREEN POSITION 55.
SELECT-OPTIONS:
       s_safra2    FOR zim01_sol_ap_inv-safra2 NO INTERVALS NO-EXTENSION .

SELECTION-SCREEN END OF LINE.
*        SELECTION-SCREEN BEGIN OF LINE.
*    SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK b0.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
PARAMETERS: p_pt1 AS CHECKBOX, "Aprovados
            p_pt2 AS CHECKBOX, "Reprovados
            p_pt3 AS CHECKBOX, "Bloqueados
            p_pt4 AS CHECKBOX DEFAULT 'X'. "Sem status
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE text-003.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(15) FOR FIELD pm_uname.
SELECTION-SCREEN POSITION 33.
PARAMETERS:
  pm_uname          LIKE sy-uname DEFAULT sy-uname.
SELECTION-SCREEN POSITION 45.
PARAMETERS:
*    leoobsf
 p_name            LIKE adrp-name_text.
SELECTION-SCREEN END OF LINE.
*  leoobsi
*  SELECTION-SCREEN BEGIN OF LINE.
**    SELECTION-SCREEN COMMENT 1(12) FOR FIELD S_RESP.
*    SELECTION-SCREEN POSITION 30.
SELECT-OPTIONS:
                s_resp       FOR zim02_sol_ap_ctl-responsavel NO-DISPLAY.
* SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK b3.

DATA: it_cont    LIKE zim02_sol_ap_ctl OCCURS 0 WITH HEADER LINE.
DATA: BEGIN OF it_final OCCURS 0.
        INCLUDE STRUCTURE zim01_sol_ap_inv.
DATA:   fase01(30),
        icon(4),
        sel,
        txtstat(15),
        name1 TYPE t001w-name1,
        ltext TYPE cskt-ltext,
*leoobsi
        responsavel TYPE uname,
*leoobsf
END OF it_final.
DATA: w_changed,
      w_leave,
      w_exit.
*ALV data declarations

DATA: cl_alv TYPE REF TO cl_gui_alv_grid.
TYPE-POOLS: slis.
DATA: fieldcatalog TYPE slis_t_fieldcat_alv WITH HEADER LINE,
      gd_tab_group TYPE slis_t_sp_group_alv,
      gd_layout    TYPE slis_layout_alv,
      gd_repid     LIKE sy-repid.

RANGES: r_status  FOR zim01_sol_ap_inv-status_aprov.


*P_PT1  Aprovados
*P_PT2  Reprovados
*P_PT3  Bloqueados
*P_PT4  Sem status

INITIALIZATION.

AT SELECTION-SCREEN OUTPUT.

  SELECT SINGLE persnumber FROM usr21 INTO w_per
    WHERE bname = sy-uname.

  SELECT SINGLE name_text FROM adrp INTO p_name
    WHERE persnumber = w_per.
*BREAK-POINT.
  LOOP AT SCREEN.
    IF screen-name = 'PM_UNAME' OR screen-name = 'P_NAME'.
      screen-input = 0.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.

START-OF-SELECTION.

  CLEAR r_status. REFRESH r_status.
  IF NOT p_pt1 IS INITIAL.
    r_status-sign = 'I'.
    r_status-option = 'EQ'.
    r_status-low  = '1'.
    APPEND r_status. CLEAR r_status.
  ENDIF.

  IF NOT p_pt2 IS INITIAL.
    r_status-sign = 'I'.
    r_status-option = 'EQ'.
    r_status-low  = '2'.
    APPEND r_status. CLEAR r_status.
  ENDIF.

  IF NOT p_pt3 IS INITIAL.
    r_status-sign = 'I'.
    r_status-option = 'EQ'.
    r_status-low  = '3'.
    APPEND r_status. CLEAR r_status.
  ENDIF.

  IF NOT p_pt4 IS INITIAL.
    r_status-sign = 'I'.
    r_status-option = 'EQ'.
    r_status-low  = ' '.
    APPEND r_status. CLEAR r_status.
  ENDIF.

  PERFORM f_get_data.
  PERFORM f_build_fields.
  PERFORM f_layout.
  PERFORM f_call_alv.

*&---------------------------------------------------------------------*
*&      Form  F_GET_DATA
*&---------------------------------------------------------------------*
FORM f_get_data .
  REFRESH: it_final, it_cont.
  SELECT *
    FROM zim02_sol_ap_ctl
    INTO TABLE it_cont
   WHERE bukrs IN so_bukrs AND
         kostl     IN so_kostl
     AND aprovador EQ pm_uname
    AND ano IN so_ano
    AND safra IN s_safra
    AND safra2 IN s_safra2
*leoobsi
    AND responsavel IN s_resp.
*leoobsf
  CHECK sy-subrc IS INITIAL.

  SELECT *
    FROM zim01_sol_ap_inv
    INTO TABLE it_final
     FOR ALL ENTRIES IN it_cont
     WHERE bukrs   EQ it_cont-bukrs
       AND gsber   IN so_gsber
       AND ano     EQ it_cont-ano
       AND safra   EQ it_cont-safra
       AND safra2  EQ it_cont-safra2
       AND kostl   EQ it_cont-kostl
       AND status_cta EQ '2'
       AND status_aprov IN r_status
       AND fase   IN so_fase
       and posnr  in so_posnr.


  DATA: BEGIN OF t_001w OCCURS 0.
          INCLUDE STRUCTURE t001w.
  DATA: END OF t_001w.

  SELECT * FROM t001w INTO TABLE t_001w.

  SORT t_001w BY werks.

*leoobsi
  SORT it_cont BY bukrs ano safra safra2 kostl.
*leoobsf

  LOOP AT it_final.
   shift it_final-posnr left deleting leading '0'.
*leoobsi
    IF it_final-ano_fim_exec IS INITIAL.
      it_final-ano_fim_exec = it_final-dt_fim(4).
    ENDIF.
    READ TABLE it_cont WITH KEY bukrs  = it_final-bukrs
                                ano    = it_final-ano
                                safra  = it_final-safra
                                safra2 = it_final-safra2
                                kostl  = it_final-kostl
                                BINARY SEARCH.
    IF sy-subrc = 0.
      MOVE it_cont-responsavel TO it_final-responsavel.
    ENDIF.

*leoobsf
    READ TABLE t_001w WITH KEY werks = it_final-gsber BINARY SEARCH.
    IF sy-subrc = 0.
      it_final-name1 = t_001w-name1.
    ENDIF.

    SELECT SINGLE ltext FROM cskt INTO it_final-ltext
      WHERE spras = sy-langu AND
            kostl = it_final-kostl.

    PERFORM f_check_status.

    CASE it_final-fase.
      WHEN 01.
        CONCATENATE '01' 'Planejado' INTO it_final-fase01 SEPARATED BY ' - '.
      WHEN 02.
        CONCATENATE '01' 'Extra' INTO it_final-fase01 SEPARATED BY ' - '.
      WHEN 03.
        CONCATENATE '01' 'Sinistro' INTO it_final-fase01 SEPARATED BY ' - '.
    ENDCASE.
    MODIFY it_final.
  ENDLOOP.
  CLEAR w_changed.
ENDFORM.                    " F_GET_DATA

*&---------------------------------------------------------------------*
*&      Form  f_build_fields
*&---------------------------------------------------------------------*

FORM f_build_fields.
  PERFORM f_add_field USING: 'ICON'           'Control'(100)                       'X' ' ' ' ',
*                             'STATUS_APROV'   'Aprovação '(101)                    ' ' ' ',it_final-txtstat = 'Aprovado'.
                             'TXTSTAT'   'Aprovação '(101)                         ' ' ' ' ' ',
                             'POSNR'          'Solic.investimento'                     ' ' ' ' ' ',
                             'BUKRS'          'Empresa'(102)                       ' ' ' ' ' ',
                             'GSBER'          'Filial'(103)                        ' ' ' ' ' ',
                             'NAME1'          'Nome da Filial'(199)                ' ' ' ' ' ',
                             'KOSTL'          'Centro Custo'(104)                  ' ' ' ' ' ',
                             'LTEXT'          'Descrição'(198)                     ' ' ' ' ' ',
                             'BUZEI'          'Seq.'(105)                          ' ' ' ' ' ',
                             'FASE01'           'Fase'(106)                          ' ' ' ' ' ',
                             'IZWEK'          'Natureza'(107)                      ' ' ' ' ' ',
                             'TXT50'          'Categoria do investimento'(108)     ' ' ' ' '60',
                             'OBJETIVO'       'Objetivo do investimento '(109)     ' ' ' ' '60',
                             'DESCR_ITEM'     'Descrição do item'(110)             ' ' ' ' '100',
                             'MENGE'          'Quant '(111)                        ' ' ' ' ' ',
                             'VLR_UNITARIO'   'Unitário R$ '(112)                     ' ' ' ' ' ',
                             'VLR_TOTAL'      'Total R$'(153)                      ' ' ' ' ' ',
                             'VL_USD'      'Total US$'(143)                        ' ' ' ' ' ',
                             'VL_EUR'      'Total EUR'(133)                        ' ' ' ' ' ',
                             'DT_INICIO'      'Dt.Inicio '(114)                    ' ' ' ' ' ',
                             'DT_FIM'         'Dt.fim '(115)                       ' ' ' ' ' ',
*leoobsi
                             'ANO_EXEC_FIM'   'Ano fim '(124)                      ' ' ' ' ' ',
*leoobsf
*                             'STATUS_CTA'     'Status Contábil'(116)               ' ' ' ',
*                             'KNTTP'          'Classif.contabil'(117)              ' ' ' ',
*                             'KNTTX'          ''(118)                              ' ' ' ',
                             'OBSERVACOES'    'Observação Contábil '(119)          ' ' ' ' ' ',
                             'TXT20'          'Descrição'(120)                     ' ' ' ' ' ',
                             'USUARIO'        'Usuario'(121)                       ' ' ' ' ' ',
                             'DATA_ENTR'      'Data Entrada'(122)                  ' ' ' ' ' ',
                             'USUARIO_IM'     'Usuario IM'(123)                    ' ' ' ' ' ',
                             'RESPONSAVEL'    'Resp. C.Custo'(125)                 ' ' ' ' ' '.

ENDFORM. "f_build_fields


*&---------------------------------------------------------------------*
*&      Form  f_layout
*&---------------------------------------------------------------------*

FORM f_layout.
  gd_layout-no_input          = 'X'.
  gd_layout-colwidth_optimize = 'X'.
  gd_layout-box_fieldname     = 'SEL'.
  gd_layout-box_tabname       = 'T_FINAL'.
ENDFORM. "f_layout


*&---------------------------------------------------------------------*
*&      Form  f_call_alv
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*

FORM f_call_alv.
  gd_repid = sy-repid.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program       = gd_repid
      i_callback_user_command  = 'F_USER_COMMAND'
      i_callback_pf_status_set = 'F_PF_STATUS'
      is_layout                = gd_layout
      it_fieldcat              = fieldcatalog[]
      i_save                   = 'A'
    TABLES
      t_outtab                 = it_final
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.
  IF sy-subrc <> 0.

* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.

  ENDIF.

ENDFORM. "f_call_alv

*&---------------------------------------------------------------------*
*&      Form  F_ADD_FIELD
*----------------------------------------------------------------------*
*      --> fieldname
*      --> Fieldtext
*----------------------------------------------------------------------*
FORM f_add_field USING p_field p_text p_icon p_edit p_outputlen.
*leoobsi
  DATA: wl_tam TYPE i.
  wl_tam = STRLEN( p_text ).
  wl_tam = wl_tam + 1.
*leoobsf
  CLEAR  fieldcatalog.
  fieldcatalog-fieldname   = p_field.
  fieldcatalog-reptext_ddic   = p_text.
  fieldcatalog-icon        = p_icon.
  fieldcatalog-edit        = p_edit.
  fieldcatalog-input       = p_edit.
*leoobsi
  IF wl_tam > p_outputlen.
    fieldcatalog-outputlen   = wl_tam.
  ELSE.
    fieldcatalog-outputlen   = p_outputlen.
  ENDIF.
*leoobsf
  APPEND fieldcatalog TO fieldcatalog.
ENDFORM. " F_ADD_FIELD


*&---------------------------------------------------------------------*
*&      Form  f_user_command
*&---------------------------------------------------------------------*

FORM f_user_command USING l_ucomm
                          l_selfield TYPE slis_selfield.
  PERFORM f_retrieve_changed_data.
  CLEAR: w_leave, w_exit.
  CASE l_ucomm.
    WHEN 'APRO'.      PERFORM f_aprova.
    WHEN 'REPR'.      PERFORM f_reprova.
    WHEN 'BLOCK'.     PERFORM f_bloqueia.
    WHEN 'SAVE'.      PERFORM f_salva.
    WHEN 'VOLTA'.     PERFORM f_back.
    WHEN 'CANC'.      PERFORM f_back.
    WHEN 'EXIT'.      PERFORM f_back.
    WHEN 'REFRE'.     PERFORM f_get_data.
  ENDCASE.
  l_selfield-col_stable   = 'X'.
  l_selfield-row_stable   = 'X'.
  l_selfield-refresh      = 'X'.
  l_selfield-after_action = 'X'.
  l_selfield-exit         = w_exit.

ENDFORM. "f_user_command

*&---------------------------------------------------------------------*
*&      Form  f_retrieve_changed_data
*&---------------------------------------------------------------------*
FORM f_retrieve_changed_data .

  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      e_grid = cl_alv.
  CALL METHOD cl_alv->check_changed_data.

ENDFORM. " F_RETRIEVE_CHANGED_DATA

*&---------------------------------------------------------------------*
*&      Form  f_pf_status
*&---------------------------------------------------------------------*
FORM f_pf_status USING t_excl TYPE slis_t_extab.
  SET PF-STATUS 'ALV' EXCLUDING t_excl.
ENDFORM. "f_pf_status
*&---------------------------------------------------------------------*
*&      Form  F_APROVA
*&---------------------------------------------------------------------*
FORM f_aprova.
  READ TABLE it_final WITH KEY sel = 'X'.
  IF NOT sy-subrc IS INITIAL.
    MESSAGE s278(57).
    EXIT.
  ENDIF.

  LOOP AT it_final WHERE sel = 'X'.

    PERFORM valida_bloqueio USING it_final-bukrs
                                  it_final-ano
                                  it_final-safra
                                  it_final-safra2
                                  it_final-kostl
                            CHANGING wg_bloqueio.

    IF NOT wg_bloqueio IS INITIAL.
      MESSAGE i836(sd) WITH 'O Exercício ' it_final-ano 'está encerrado para investimentos.'.
      EXIT.
    ENDIF.
*                     AND status_aprov NE '2'.
    it_final-status_aprov  = '1'.
    it_final-dt_aprovacao  = sy-datum.
    it_final-aprovador     = pm_uname.
    PERFORM f_check_status.

    MODIFY it_final.
    w_changed = 'X'.
  ENDLOOP.


ENDFORM.                    " F_APROVA
*&---------------------------------------------------------------------*
*&      Form  F_REPROVA
*&---------------------------------------------------------------------*
FORM f_reprova .
  READ TABLE it_final WITH KEY sel = 'X'.
  IF NOT sy-subrc IS INITIAL.
    MESSAGE s278(57).
    EXIT.
  ENDIF.

  LOOP AT it_final WHERE sel = 'X'
                     AND status_aprov NE '2'.

    PERFORM valida_bloqueio USING it_final-bukrs
                                  it_final-ano
                                  it_final-safra
                                  it_final-safra2
                                  it_final-kostl
                            CHANGING wg_bloqueio.

    IF NOT wg_bloqueio IS INITIAL.
      MESSAGE i836(sd) WITH 'O Exercício ' it_final-ano ' está Encerrado para Investimentos.'.
      EXIT.
    ENDIF.

    IF it_final-status_aprov = '1' OR it_final-status_aprov = '3' .
      IF NOT it_final-posnr IS INITIAL  .
        SELECT SINGLE posnr FROM imaka INTO zim01_sol_ap_inv-posnr
          WHERE posnr = it_final-posnr.
        IF sy-subrc = 0.

          MESSAGE i836(sd) WITH 'Já existe imobilizado/ordem interna para'
                                'está solicitação de investimento somente'
                                'pode ser “Bloqueada”.'.
          EXIT.
        ENDIF.

      ENDIF.
    ENDIF.
    it_final-status_aprov  = '2'.
    it_final-dt_aprovacao  = sy-datum.
    it_final-aprovador     = pm_uname.
    PERFORM f_check_status.
    MODIFY it_final.
    w_changed = 'X'.
  ENDLOOP.

ENDFORM.                    " F_REPROVA
*&---------------------------------------------------------------------*
*&      Form  F_BLOQUEIA
*&---------------------------------------------------------------------*
FORM f_bloqueia .
  READ TABLE it_final WITH KEY sel = 'X'.
  IF NOT sy-subrc IS INITIAL.
    MESSAGE s278(57).
    EXIT.
  ENDIF.

  LOOP AT it_final WHERE sel = 'X'.
*                     AND status_aprov NE '2'.
    PERFORM valida_bloqueio USING it_final-bukrs
                                  it_final-ano
                                  it_final-safra
                                  it_final-safra2
                                  it_final-kostl
                            CHANGING wg_bloqueio.

    IF NOT wg_bloqueio IS INITIAL.
      MESSAGE i836(sd) WITH 'O Exercício ' it_final-ano ' está Encerrado para Investimentos.'.
      EXIT.
    ENDIF.
    it_final-status_aprov  = '3'.
    it_final-dt_aprovacao  = sy-datum.
    it_final-aprovador     = pm_uname.
    PERFORM f_check_status.
    MODIFY it_final.
    w_changed = 'X'.
  ENDLOOP.

ENDFORM.                    " F_BLOQUEIA
*&---------------------------------------------------------------------*
*&      Form  F_CHECK_STATUS
*&---------------------------------------------------------------------*
FORM f_check_status .
  CASE it_final-status_aprov.
    WHEN '1'.
      it_final-icon = icon_system_okay.
      it_final-txtstat = 'Aprovado'.
    WHEN '2'.
      it_final-icon = icon_message_critical.
      it_final-txtstat = 'Reprovado'.
    WHEN '3'.
      it_final-icon = icon_locked.
      it_final-txtstat = 'Bloqueado'.
    WHEN OTHERS. it_final-icon = icon_message_question_small.
  ENDCASE.
ENDFORM.                    " F_CHECK_STATUS
*&---------------------------------------------------------------------*
*&      Form  F_SALVA
*&---------------------------------------------------------------------*
FORM f_salva .
  CHECK NOT it_final[] IS INITIAL.
  MODIFY zim01_sol_ap_inv FROM TABLE it_final.
  COMMIT WORK AND WAIT.

  CLEAR w_changed.
ENDFORM.                    " F_SALVA

*&---------------------------------------------------------------------*
*&      Form  F_BACK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_back.
  w_exit   = 'X'.
  PERFORM f_confirm_exit.
  CASE w_leave.
    WHEN  'C'.
      CLEAR w_exit.
      EXIT.
    WHEN  'N'.
      PERFORM f_salva.
  ENDCASE.


ENDFORM.                    "F_BACK
*&---------------------------------------------------------------------*
*&      Form  F_CONFIRM_EXIT
*&---------------------------------------------------------------------*
FORM f_confirm_exit .
  CLEAR w_leave.
  IF NOT w_changed IS INITIAL.

    CALL FUNCTION 'BM_POPUP_TO_CONFIRM_EXIT'
      EXPORTING
        popup_title = 'Solicitação de Investimentos'(210)
      IMPORTING
        answer      = w_leave
      EXCEPTIONS
        OTHERS      = 2.
    IF sy-subrc <> 0.
      w_leave = 'C'.
    ENDIF.

  ENDIF.

ENDFORM.                    " F_CONFIRM_EXIT
*&---------------------------------------------------------------------*
*&      Form  VALIDA_BLOQUEIO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_FINAL_BUKRS  text
*      -->P_IT_FINAL_ANO  text
*      -->P_IT_FINAL_SAFRA  text
*      -->P_IT_FINAL_SAFRA2  text
*      -->P_IT_FINAL_KOSTL  text
*      <--P_WG_BLOQUEIO  text
*----------------------------------------------------------------------*
FORM valida_bloqueio  USING    p_bukrs
                               p_ano
                               p_safra
                               p_safra2
                               p_kostl
                      CHANGING ch_bloqueio.

  CLEAR: ch_bloqueio.

  SELECT SINGLE blq_exerc FROM zim02_sol_ap_ctl INTO ch_bloqueio
    WHERE bukrs  = p_bukrs  AND
          ano    = p_ano    AND
          safra  = p_safra  AND
          safra2 = p_safra2 AND
          kostl  = p_kostl.

ENDFORM.                    " VALIDA_BLOQUEIO

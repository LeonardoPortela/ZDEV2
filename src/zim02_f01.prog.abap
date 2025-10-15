*&---------------------------------------------------------------------*
*&  Include           ZIM02_F01
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
*   INCLUDE TABLECONTROL_FORMS                                         *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  USER_OK_TC                                               *
*&---------------------------------------------------------------------*
 FORM user_ok_tc USING    p_tc_name TYPE dynfnam
                          p_table_name
                          p_mark_name
                 CHANGING p_ok      LIKE sy-ucomm.

*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
   DATA: l_ok              TYPE sy-ucomm,
         l_offset          TYPE i.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

*&SPWIZARD: Table control specific operations                          *
*&SPWIZARD: evaluate TC name and operations                            *
   SEARCH p_ok FOR p_tc_name.
   IF sy-subrc <> 0.
     EXIT.
   ENDIF.
   l_offset = STRLEN( p_tc_name ) + 1.
   l_ok = p_ok+l_offset.
*&SPWIZARD: execute general and TC specific operations                 *
   CASE l_ok.
     WHEN 'INSR'.                      "insert row
       IF w_up EQ 'V'.
         MESSAGE i000(z01) WITH 'Período Fechado para Planejamento'.
         LEAVE SCREEN.
       ENDIF.
       PERFORM fcode_insert_row USING    p_tc_name
                                         p_table_name.
       CLEAR p_ok.

     WHEN 'DELE'.                      "delete row
       IF w_up EQ 'V'.
         MESSAGE i000(z01) WITH 'Período Fechado para Planejamento'.
         LEAVE SCREEN.
       ENDIF.
       PERFORM fcode_delete_row USING    p_tc_name
                                         p_table_name
                                         p_mark_name.
       CLEAR p_ok.

     WHEN 'P--' OR                     "top of list
          'P-'  OR                     "previous page
          'P+'  OR                     "next page
          'P++'.                       "bottom of list
       PERFORM compute_scrolling_in_tc USING p_tc_name
                                             l_ok.
       CLEAR p_ok.
*     WHEN 'L--'.                       "total left
*       PERFORM FCODE_TOTAL_LEFT USING P_TC_NAME.
*
*     WHEN 'L-'.                        "column left
*       PERFORM FCODE_COLUMN_LEFT USING P_TC_NAME.
*
*     WHEN 'R+'.                        "column right
*       PERFORM FCODE_COLUMN_RIGHT USING P_TC_NAME.
*
*     WHEN 'R++'.                       "total right
*       PERFORM FCODE_TOTAL_RIGHT USING P_TC_NAME.
*
     WHEN 'MARK'.                      "mark all filled lines
       PERFORM fcode_tc_mark_lines USING p_tc_name
                                         p_table_name
                                         p_mark_name   .
       CLEAR p_ok.

     WHEN 'DMRK'.                      "demark all filled lines
       PERFORM fcode_tc_demark_lines USING p_tc_name
                                           p_table_name
                                           p_mark_name .
       CLEAR p_ok.

*     WHEN 'SASCEND'   OR
*          'SDESCEND'.                  "sort column
*       PERFORM FCODE_SORT_TC USING P_TC_NAME
*                                   l_ok.

   ENDCASE.

 ENDFORM.                              " USER_OK_TC

*&---------------------------------------------------------------------*
*&      Form  FCODE_INSERT_ROW                                         *
*&---------------------------------------------------------------------*
 FORM fcode_insert_row
               USING    p_tc_name           TYPE dynfnam
                        p_table_name             .

*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
   DATA l_lines_name       LIKE feld-name.
   DATA l_selline          LIKE sy-stepl.
   DATA l_lastline         TYPE i.
   DATA l_line             TYPE i.
   DATA: l_field(30)       TYPE c.
   DATA l_table_name       LIKE feld-name.
   FIELD-SYMBOLS <tc>                 TYPE cxtab_control.
   FIELD-SYMBOLS <table>              TYPE STANDARD TABLE.
   FIELD-SYMBOLS <lines>              TYPE i.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

   ASSIGN (p_tc_name) TO <tc>.

*&SPWIZARD: get the table, which belongs to the tc                     *
   CONCATENATE p_table_name '[]' INTO l_table_name. "table body
   ASSIGN (l_table_name) TO <table>.                "not headerline

*&SPWIZARD: get looplines of TableControl                              *
   CONCATENATE 'G_' p_tc_name '_LINES' INTO l_lines_name.
   ASSIGN (l_lines_name) TO <lines>.

*&SPWIZARD: get current line
   GET CURSOR FIELD l_field.
   GET CURSOR LINE l_selline.
   IF sy-subrc <> 0.                   " append line to table
     l_selline = <tc>-lines + 1.
*&SPWIZARD: set top line                                               *
     IF l_selline > <lines>.
       <tc>-top_line = l_selline - <lines> + 1 .
     ELSE.
       <tc>-top_line = 1.
     ENDIF.
   ELSE.                               " insert line into table
     l_selline = <tc>-top_line + l_selline - 1.
     l_lastline = <tc>-top_line + <lines> - 1.
   ENDIF.
*&SPWIZARD: set new cursor line                                        *
   l_line = l_selline - <tc>-top_line + 1.

*&SPWIZARD: insert initial line                                        *
   INSERT INITIAL LINE INTO <table> INDEX l_selline.
   <tc>-lines = <tc>-lines + 1.
*&SPWIZARD: set cursor                                                 *
*   SET CURSOR LINE l_line.
*   SET CURSOR FIELD l_field LINE l_line.

 ENDFORM.                              " FCODE_INSERT_ROW

*&---------------------------------------------------------------------*
*&      Form  FCODE_DELETE_ROW                                         *
*&---------------------------------------------------------------------*
 FORM fcode_delete_row
               USING    p_tc_name           TYPE dynfnam
                        p_table_name
                        p_mark_name   .

*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
   DATA l_table_name       LIKE feld-name.

   FIELD-SYMBOLS <tc>         TYPE cxtab_control.
   FIELD-SYMBOLS <table>      TYPE STANDARD TABLE.
   FIELD-SYMBOLS <wa>.
   FIELD-SYMBOLS <mark_field>.
   FIELD-SYMBOLS <fase>.
   FIELD-SYMBOLS <status_cta>.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

   ASSIGN (p_tc_name) TO <tc>.

*&SPWIZARD: get the table, which belongs to the tc                     *
   CONCATENATE p_table_name '[]' INTO l_table_name. "table body
   ASSIGN (l_table_name) TO <table>.                "not headerline

*&SPWIZARD: delete marked lines                                        *
   DESCRIBE TABLE <table> LINES <tc>-lines.

   LOOP AT <table> ASSIGNING <wa>.

*&SPWIZARD: access to the component 'FLAG' of the table header         *
     ASSIGN COMPONENT p_mark_name OF STRUCTURE <wa> TO <mark_field>.

     IF <mark_field> = 'X'.
       ASSIGN COMPONENT 'FASE' OF STRUCTURE <wa> TO <fase>.
       ASSIGN COMPONENT 'STATUS_CTA' OF STRUCTURE <wa> TO <status_cta>.

*       IF ( <fase> EQ 1 OR
*            <fase> EQ 3    )." AND
*         MESSAGE i000(z01) WITH 'Item não pode ser excluido' .
*         CONTINUE.
*       ENDIF.
*** Modificação - Eduardo Ruttkowski Tavares - 30.08.2011 >>> INI
*       IF ( <status_cta> EQ 1 OR
*            <status_cta> EQ 2   ).

       IF NOT <status_cta> IS INITIAL.
*** Modificação - Eduardo Ruttkowski Tavares - 30.08.2011 <<< FIM
         MESSAGE i000(z01) WITH 'Item não pode ser excluido' .
         CONTINUE.

       ENDIF.
       DELETE t_texto WHERE seqnum = syst-tabix.
       DELETE <table> INDEX syst-tabix.

       IF sy-subrc = 0.
         <tc>-lines = <tc>-lines - 1.
       ENDIF.
     ENDIF.
   ENDLOOP.

 ENDFORM.                              " FCODE_DELETE_ROW

*&---------------------------------------------------------------------*
*&      Form  COMPUTE_SCROLLING_IN_TC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TC_NAME  name of tablecontrol
*      -->P_OK       ok code
*----------------------------------------------------------------------*
 FORM compute_scrolling_in_tc USING    p_tc_name
                                       p_ok.
*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
   DATA l_tc_new_top_line     TYPE i.
   DATA l_tc_name             LIKE feld-name.
   DATA l_tc_lines_name       LIKE feld-name.
   DATA l_tc_field_name       LIKE feld-name.

   FIELD-SYMBOLS <tc>         TYPE cxtab_control.
   FIELD-SYMBOLS <lines>      TYPE i.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

   ASSIGN (p_tc_name) TO <tc>.
*&SPWIZARD: get looplines of TableControl                              *
   CONCATENATE 'G_' p_tc_name '_LINES' INTO l_tc_lines_name.
   ASSIGN (l_tc_lines_name) TO <lines>.


*&SPWIZARD: is no line filled?                                         *
   IF <tc>-lines = 0.
*&SPWIZARD: yes, ...                                                   *
     l_tc_new_top_line = 1.
   ELSE.
*&SPWIZARD: no, ...                                                    *
     CALL FUNCTION 'SCROLLING_IN_TABLE'
          EXPORTING
               entry_act             = <tc>-top_line
               entry_from            = 1
               entry_to              = <tc>-lines
               last_page_full        = 'X'
               loops                 = <lines>
               ok_code               = p_ok
               overlapping           = 'X'
          IMPORTING
               entry_new             = l_tc_new_top_line
          EXCEPTIONS
*              NO_ENTRY_OR_PAGE_ACT  = 01
*              NO_ENTRY_TO           = 02
*              NO_OK_CODE_OR_PAGE_GO = 03
               OTHERS                = 0.
   ENDIF.

*&SPWIZARD: get actual tc and column                                   *
   GET CURSOR FIELD l_tc_field_name
              AREA  l_tc_name.

   IF syst-subrc = 0.
     IF l_tc_name = p_tc_name.
*&SPWIZARD: et actual column                                           *
       SET CURSOR FIELD l_tc_field_name LINE 1.
     ENDIF.
   ENDIF.

*&SPWIZARD: set the new top line                                       *
   <tc>-top_line = l_tc_new_top_line.


 ENDFORM.                              " COMPUTE_SCROLLING_IN_TC

*&---------------------------------------------------------------------*
*&      Form  FCODE_TC_MARK_LINES
*&---------------------------------------------------------------------*
*       marks all TableControl lines
*----------------------------------------------------------------------*
*      -->P_TC_NAME  name of tablecontrol
*----------------------------------------------------------------------*
 FORM fcode_tc_mark_lines USING p_tc_name
                                p_table_name
                                p_mark_name.
*&SPWIZARD: EGIN OF LOCAL DATA-----------------------------------------*
   DATA l_table_name       LIKE feld-name.

   FIELD-SYMBOLS <tc>         TYPE cxtab_control.
   FIELD-SYMBOLS <table>      TYPE STANDARD TABLE.
   FIELD-SYMBOLS <wa>.
   FIELD-SYMBOLS <mark_field>.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

   ASSIGN (p_tc_name) TO <tc>.

*&SPWIZARD: get the table, which belongs to the tc                     *
   CONCATENATE p_table_name '[]' INTO l_table_name. "table body
   ASSIGN (l_table_name) TO <table>.                "not headerline

*&SPWIZARD: mark all filled lines                                      *
   LOOP AT <table> ASSIGNING <wa>.

*&SPWIZARD: access to the component 'FLAG' of the table header         *
     ASSIGN COMPONENT p_mark_name OF STRUCTURE <wa> TO <mark_field>.

     <mark_field> = 'X'.
   ENDLOOP.
 ENDFORM.                                          "fcode_tc_mark_lines

*&---------------------------------------------------------------------*
*&      Form  FCODE_TC_DEMARK_LINES
*&---------------------------------------------------------------------*
*       demarks all TableControl lines
*----------------------------------------------------------------------*
*      -->P_TC_NAME  name of tablecontrol
*----------------------------------------------------------------------*
 FORM fcode_tc_demark_lines USING p_tc_name
                                  p_table_name
                                  p_mark_name .
*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
   DATA l_table_name       LIKE feld-name.

   FIELD-SYMBOLS <tc>         TYPE cxtab_control.
   FIELD-SYMBOLS <table>      TYPE STANDARD TABLE.
   FIELD-SYMBOLS <wa>.
   FIELD-SYMBOLS <mark_field>.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

   ASSIGN (p_tc_name) TO <tc>.

*&SPWIZARD: get the table, which belongs to the tc                     *
   CONCATENATE p_table_name '[]' INTO l_table_name. "table body
   ASSIGN (l_table_name) TO <table>.                "not headerline

*&SPWIZARD: demark all filled lines                                    *
   LOOP AT <table> ASSIGNING <wa>.

*&SPWIZARD: access to the component 'FLAG' of the table header         *
     ASSIGN COMPONENT p_mark_name OF STRUCTURE <wa> TO <mark_field>.

     <mark_field> = space.
   ENDLOOP.
 ENDFORM.                                          "fcode_tc_mark_lines
*&---------------------------------------------------------------------*
*&      Form  F_BUSCA_DADOS
*&---------------------------------------------------------------------*
 FORM f_busca_dados .

   SELECT SINGLE * FROM csks INTO csks
     WHERE kostl = zim01_sol_ap_inv-kostl.

   IF sy-subrc <> 0.
     MESSAGE i000(zfi) WITH 'Planejamento já existe'.
     LEAVE SCREEN.
   ENDIF.


   SELECT * FROM zim01_textos INTO TABLE t_texto
     WHERE kostl = zim01_sol_ap_inv-kostl AND
           gjahr = zim01_sol_ap_inv-ano   AND
           safra = zim01_sol_ap_inv-safra AND
          safra2 = zim01_sol_ap_inv-safra2.

   SELECT SINGLE butxt FROM t001 INTO w_bukrst
     WHERE bukrs = csks-bukrs.

   SELECT SINGLE name FROM j_1bbranch INTO w_gsbert
     WHERE bukrs  = csks-bukrs AND
           branch = csks-gsber.
   IF sy-tcode = 'ZIM02'.
     SELECT SINGLE * FROM zim02_sol_ap_ctl INTO zim02_sol_ap_ctl
       WHERE ano   = zim01_sol_ap_inv-ano   AND
*           safra = zim01_sol_ap_inv-safra AND
*           safra2 = zim01_sol_ap_inv-safra2 AND
             kostl = zim01_sol_ap_inv-kostl AND
            ( dt_aprov_in  LE sy-datum AND
              dt_aprov_fim GE sy-datum     ).

     IF sy-subrc <> 0.
       w_up = 'V'.
       SELECT SINGLE * FROM zim02_sol_ap_ctl INTO zim02_sol_ap_ctl
         WHERE ano   = zim01_sol_ap_inv-ano   AND
*           safra = zim01_sol_ap_inv-safra AND
*           safra2 = zim01_sol_ap_inv-safra2 AND
               kostl = zim01_sol_ap_inv-kostl." AND
*            ( dt_aprov_in  LE sy-datum AND
*              dt_aprov_fim GE sy-datum     ).
       IF sy-ucomm EQ 'CRIA'.
         MESSAGE i000(z01) WITH 'Período Fechado para Planejamento'.
         LEAVE SCREEN.
       ENDIF.
     ELSE.
       CLEAR w_up.
*       LEAVE SCREEN.
     ENDIF.
   ELSE.

     SELECT SINGLE * FROM zim02_sol_ap_ctl INTO zim02_sol_ap_ctl
       WHERE ano   = zim01_sol_ap_inv-ano   AND
*           safra = zim01_sol_ap_inv-safra AND
*           safra2 = zim01_sol_ap_inv-safra2 AND
             kostl = zim01_sol_ap_inv-kostl." AND
     "( dt_aprov_in  LE sy-datum AND
     " dt_aprov_fim GE sy-datum     ).
   ENDIF.

   IF zim01_sol_ap_inv-safra NE zim02_sol_ap_ctl-safra
     AND NOT zim01_sol_ap_inv-safra IS INITIAL.
     MESSAGE i000(z01) WITH 'Safra início inválida'.
     LEAVE SCREEN.
   ENDIF.

   IF zim01_sol_ap_inv-safra2 NE zim02_sol_ap_ctl-safra2
     AND NOT zim01_sol_ap_inv-safra2 IS INITIAL.
     MESSAGE i000(z01) WITH 'Safra fim inválida'.
     LEAVE SCREEN.
   ENDIF.

   SELECT * FROM zim01_sol_ap_inv INTO TABLE t_inv
     WHERE ano   = zim01_sol_ap_inv-ano      AND
           safra = zim02_sol_ap_ctl-safra    AND
           safra2  = zim02_sol_ap_ctl-safra2 AND
           kostl = zim01_sol_ap_inv-kostl.

   CASE sy-ucomm.

     WHEN 'CRIA'.
       IF sy-subrc = 0.
         MESSAGE i000(z01) WITH 'Planejamento já existe'.
         LEAVE SCREEN.
       ELSE.
         w_up = 'X'.
         CALL SCREEN 110.
       ENDIF.
     WHEN 'MODI' OR 'VIEW'.
       IF sy-subrc <> 0.
         MESSAGE i000(z01) WITH 'Planejamento não existente'.
         LEAVE SCREEN.
       ELSE.
*         w_up = ' '.
         CALL SCREEN 110.
       ENDIF.
   ENDCASE.
 ENDFORM.                    " F_BUSCA_DADOS
*&---------------------------------------------------------------------*
*&      Form  F_SALVA_DADOS
*&---------------------------------------------------------------------*
 FORM f_salva_dados .
   DATA: wl_check TYPE zim010-cod_lib.
   DATA: w_aprov TYPE zim01_sol_ap_inv-aprovador.

   IF sy-tcode EQ 'ZIM02'.
     LOOP AT t_inv WHERE cod_gpo IS INITIAL.
       IF t_inv-cod_item IS NOT INITIAL
       OR t_inv-objetivo IS NOT INITIAL
       OR t_inv-descr_item IS NOT INITIAL
       OR t_inv-menge      IS NOT INITIAL
       OR t_inv-vlr_unitario IS NOT INITIAL
       OR t_inv-dt_inicio    IS NOT INITIAL
       OR t_inv-dt_fim       IS NOT INITIAL
       OR t_inv-izwek        IS NOT INITIAL
       OR t_inv-finalidade   IS NOT INITIAL.
         MESSAGE i000(z01) WITH 'Preenchimento obrigatório do campo'
                                '"código do grupo"!'.
         LEAVE SCREEN.

       ENDIF.
     ENDLOOP.
   ENDIF.
   IF NOT t_inv[] IS INITIAL.
     CLEAR w_save.
     LOOP AT t_inv.
       t_inv-buzei = sy-tabix.
       t_inv-ano = zim01_sol_ap_inv-ano.
       t_inv-kostl = csks-kostl.
       t_inv-gsber = csks-gsber.
       t_inv-bukrs = csks-bukrs.
       t_inv-safra = zim02_sol_ap_ctl-safra.
       t_inv-safra2 = zim02_sol_ap_ctl-safra2.
*       IF w_cria EQ 'X'.
*         t_inv-data_entr = sy-datum.
*         t_inv-hora_entr = w_hora.
*       ENDIF.
* Início Alteração Ricardo Furst.
*       IF sy-tcode = 'ZIM02'.
*           IF t_inv-hora_entr IS INITIAL.
*             t_inv-data_entr = sy-datum.
*             t_inv-hora_entr = w_hora.
*           ELSE.
*             t_inv-data_mod = sy-datum.
*             t_inv-hora_mod = w_hora.
*           ENDIF.
*       ENDIF.
* Fim Alteração Ricardo Furst.

       DELETE t_inv WHERE        izwek        IS INITIAL AND
                                 objetivo     IS INITIAL AND
                                 descr_item   IS INITIAL AND
                                 menge        IS INITIAL AND
                                 vlr_unitario IS INITIAL AND
                                 dt_inicio    IS INITIAL AND
                                 dt_fim       IS INITIAL.

       IF sy-tcode EQ 'ZIM02'.
         IF t_inv-izwek        IS INITIAL OR
            t_inv-objetivo     IS INITIAL OR
            t_inv-descr_item   IS INITIAL OR
            t_inv-menge        IS INITIAL OR
            t_inv-vlr_unitario IS INITIAL OR
            t_inv-dt_inicio    IS INITIAL OR
            t_inv-cod_gpo      IS INITIAL OR
*            t_inv-cod_item     IS INITIAL OR
            t_inv-dt_fim       IS INITIAL.
           MESSAGE i000(z01) WITH 'Preenchimento obrigatório de todos os campos!'.
           LEAVE SCREEN.
         ENDIF.
       ENDIF.
       IF sy-tcode EQ 'ZIM02'
       AND t_inv-cod_item IS INITIAL.
         READ TABLE tg_11 WITH KEY cod_item = t_inv-cod_item
                                   cod_gpo  = t_inv-cod_gpo.
         IF sy-subrc = 0.
           t_inv-status_cta  = tg_11-status_cta.
           t_inv-knttp       = tg_11-knttp.
           t_inv-knttx       = tg_11-knttx.
           t_inv-observacoes = tg_11-observacoes.
           t_inv-saknr       = tg_11-saknr.
           t_inv-txt20       = tg_11-txt20.

           CLEAR wl_check.
           SELECT SINGLE cod_lib FROM zim010 INTO wl_check
             WHERE cod_gpo = t_inv-cod_gpo.
           IF NOT wl_check IS INITIAL.
             CLEAR tg_11-preco_item.
           ENDIF.

           t_inv-vlr_unitario = tg_11-preco_item.
         ENDIF.
       ENDIF.
       MODIFY t_inv.
       clear: t_inv.
     ENDLOOP.


     LOOP AT t_texto.
       READ TABLE t_inv INDEX t_texto-seqnum.
       IF sy-subrc <> 0.
         DELETE t_texto.
       ENDIF.
     ENDLOOP.

     DELETE FROM zim01_textos WHERE kostl  = zim01_sol_ap_inv-kostl AND
                                    gjahr  = zim01_sol_ap_inv-ano   AND
                                    safra  = zim01_sol_ap_inv-safra AND
                                    safra2 = zim01_sol_ap_inv-safra2.

     COMMIT WORK AND WAIT.
     MODIFY zim01_textos FROM TABLE t_texto.

     LOOP AT t_inv.

       CLEAR w_aprov.
       SELECT SINGLE aprovador FROM zim01_sol_ap_inv
         INTO w_aprov
         WHERE bukrs = t_inv-bukrs   AND
               gsber = t_inv-gsber   AND
               ano = t_inv-ano       AND
               safra = t_inv-safra   AND
               safra2 = t_inv-safra2 AND
               kostl = t_inv-kostl   AND
               buzei = t_inv-buzei.
       IF sy-subrc <> 0.
         CLEAR t_inv-aprovador.
       ELSE.
         t_inv-aprovador = w_aprov.
       ENDIF.
       MODIFY t_inv.
     ENDLOOP.

     DELETE FROM zim01_sol_ap_inv WHERE ano   = zim01_sol_ap_inv-ano AND
                                        safra = zim01_sol_ap_inv-safra AND
                                        kostl = zim01_sol_ap_inv-kostl.
     COMMIT WORK AND WAIT.

     MODIFY zim01_sol_ap_inv FROM TABLE t_inv.
   ELSE.
     DELETE FROM zim01_sol_ap_inv WHERE ano   = zim01_sol_ap_inv-ano AND
                                        safra = zim01_sol_ap_inv-safra AND
                                        kostl = zim01_sol_ap_inv-kostl.
   ENDIF.
   COMMIT WORK AND WAIT.

   MESSAGE s000(z01) WITH 'Documento salvo'.

   CLEAR w_up.

 ENDFORM.                    " F_SALVA_DADOS
*&---------------------------------------------------------------------*
*&      Form  F_APAGA_REGISTROS
*&---------------------------------------------------------------------*
 FORM f_apaga_registros .

   DATA: w_check TYPE zim01_sol_ap_inv.

   SELECT SINGLE * FROM zim01_sol_ap_inv
     INTO zim01_sol_ap_inv
     WHERE ano   = zim01_sol_ap_inv-ano   AND
           safra = zim01_sol_ap_inv-safra AND
           kostl = zim01_sol_ap_inv-kostl.

   IF sy-subrc <> 0.
     MESSAGE i000(z01) WITH 'Planejamento não cadastrado'.
     LEAVE SCREEN.
   ENDIF.

   SELECT SINGLE * FROM zim01_sol_ap_inv
     INTO w_check
     WHERE ano   = zim01_sol_ap_inv-ano   AND
           safra = zim01_sol_ap_inv-safra AND
           kostl = zim01_sol_ap_inv-kostl AND
           status_cta NE space.
   IF sy-subrc = 0.
     MESSAGE i000(z01) WITH 'Planejamento não pode ser eliminado'.
     LEAVE SCREEN.
   ENDIF.

   CALL FUNCTION 'POPUP_TO_CONFIRM'
     EXPORTING
       titlebar              = 'Apagar'
       text_question         = 'Deseja apagar o planejamento?'
       text_button_1         = 'Sim'
       icon_button_1         = 'ICON_OKAY'
       text_button_2         = 'Não'
       icon_button_2         = 'ICON_CANCEL'
       default_button        = '1'
       display_cancel_button = ' '
     IMPORTING
       answer                = v_answer.
   IF v_answer NE '1'.
     LEAVE SCREEN.
   ENDIF.

   DELETE FROM zim01_sol_ap_inv WHERE ano   = zim01_sol_ap_inv-ano AND
                                      safra = zim01_sol_ap_inv-safra AND
                                      kostl = zim01_sol_ap_inv-kostl.
   CLEAR zim01_sol_ap_inv.
   IF sy-subrc = 0.
     MESSAGE i000(z01) WITH 'Registro eliminado com sucesso!'.
   ELSE.
     MESSAGE i000(z01) WITH 'Erro ao apagar o registro'.
     LEAVE SCREEN.
   ENDIF.

   COMMIT WORK AND WAIT.

 ENDFORM.                    " F_APAGA_REGISTROS
*&---------------------------------------------------------------------*
*&      Form  F_VOLTA
*&---------------------------------------------------------------------*
 FORM f_volta .

   DATA: program TYPE sy-repid,
         tcode   TYPE sy-tcode.

   IF w_save EQ 'X'.
     CLEAR: v_answer, w_up.
     CALL FUNCTION 'POPUP_TO_CONFIRM'
       EXPORTING
         titlebar              = 'Salvar'
         text_question         = 'Deseja salvar o planejamento?'
         text_button_1         = 'Sim'
         icon_button_1         = 'ICON_OKAY'
         text_button_2         = 'Não'
         icon_button_2         = 'ICON_CANCEL'
         default_button        = '1'
         display_cancel_button = ' '
       IMPORTING
         answer                = v_answer.
     IF v_answer NE '1'.
       CLEAR v_append.
       LEAVE TO SCREEN 100.
     ELSE.
       PERFORM f_salva_dados.
       CLEAR v_append.
       LEAVE TO SCREEN 100.
     ENDIF.
   ELSE.
     CLEAR v_append.
     LEAVE TO SCREEN 100.
   ENDIF.
*  GET PARAMETER ID 'MEN' FIELD program.
*  SET PARAMETER ID 'MEN' FIELD ''.
*
*  CLEAR: zaa_controle_doc, zaa_controle_hip.
*  IF program ='ZAA02'.
*    LEAVE PROGRAM.
*  ELSE.
*    LEAVE TO SCREEN 100.
*  ENDIF.


 ENDFORM.                    " F_VOLTA
*&---------------------------------------------------------------------*
*&      Form  MONTA_TEXTO
*&---------------------------------------------------------------------*

 FORM monta_texto  USING    p_up
                            p_line.
   DATA: w_mode TYPE c.
   DATA: tg_texto TYPE catsxt_longtext_itab.                " OCCURS 0.

   REFRESH tg_texto.

   DATA: wl_texto TYPE LINE OF catsxt_longtext_itab,
         wl_char TYPE txline.


   SORT t_texto BY kostl gjahr safra safra2 seqnum linnum.

   LOOP AT t_texto WHERE seqnum = p_line.
     wl_char = t_texto-message(72).
     wl_texto = wl_char.
     APPEND wl_texto TO tg_texto.
   ENDLOOP.

   CASE p_up.
     WHEN ''.
       w_mode = 'X'.
     WHEN OTHERS.
       CLEAR w_mode.
   ENDCASE.

   CALL FUNCTION 'CATSXT_SIMPLE_TEXT_EDITOR'
     EXPORTING
       im_title        = 'Criar nota'
       im_display_mode = w_mode
     CHANGING
       ch_text         = tg_texto.

   DELETE t_texto WHERE seqnum = p_line.

*  CLEAR tg_textos. REFRESH tg_textos.
*
   LOOP AT tg_texto INTO wl_texto.
     t_texto-message(72) = wl_texto.
     t_texto-kostl = zim01_sol_ap_inv-kostl.
     t_texto-gjahr = zim01_sol_ap_inv-ano.
     t_texto-safra = zim01_sol_ap_inv-safra.
     t_texto-safra2 = zim01_sol_ap_inv-safra2.
     t_texto-seqnum = p_line.
     t_texto-linnum = sy-tabix.
     APPEND t_texto.
   ENDLOOP.

*   SELECT * FROM ZIM01_TEXTOS INTO TABLE T_TEXTO
*     WHERE KOSTL = ZIM01_SOL_AP_INV-KOSTL AND
*           GJAHR = ZIM01_SOL_AP_INV-ANO   AND
*           SAFRA = ZIM01_SOL_AP_INV-SAFRA AND
*          SAFRA2 = ZIM01_SOL_AP_INV-SAFRA2.
 ENDFORM.                    " MONTA_TEXTO

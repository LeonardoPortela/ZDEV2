*&---------------------------------------------------------------------*
*& Report  ZSEG02
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  zseg02.

*-----------------------------------------------------------------------
* Type-pools
*-----------------------------------------------------------------------
TYPE-POOLS: slis.

TABLES: zse01_apropr_seg.

DATA: BEGIN OF t_se01 OCCURS 0.
        INCLUDE STRUCTURE zse01_apropr_seg.
DATA: sel(1).
DATA: END OF t_se01.


DATA: it_field      TYPE TABLE OF lvc_s_fcat,
      ref_container TYPE REF TO cl_gui_docking_container,
      ref_alv       TYPE REF TO cl_gui_alv_grid,
      it_sort1      TYPE TABLE OF lvc_s_sort,
      w_field       TYPE lvc_s_fcat,
      v_exclu       TYPE ui_functions,
      v_exclu1      LIKE v_exclu WITH HEADER LINE,
      ref_layout    TYPE lvc_s_layo.
*----------------------------------------------------------------------*
*   data definition
*----------------------------------------------------------------------*
*       Batchinputdata of single transaction
DATA:   bdcdata LIKE bdcdata    OCCURS 0 WITH HEADER LINE.
*       messages of call transaction
DATA:   messtab LIKE bdcmsgcoll OCCURS 0 WITH HEADER LINE.
*       error session opened (' ' or 'X')
DATA:   e_group_opened.
*       message texts
TABLES: t100.
DATA: v_repid LIKE sy-repid.
*-----------------------------------------------------------------------
* Select-options e Parameters
*-----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK  b1 WITH FRAME TITLE text-h01.
SELECT-OPTIONS: s_bukrs FOR zse01_apropr_seg-bukrs OBLIGATORY,
                s_chave FOR zse01_apropr_seg-chave
                            MATCHCODE OBJECT zseg02_chave
                            OBLIGATORY.
*PARAMETERS: p_pasta(12) OBLIGATORY.                         "Pasta SM35
SELECTION-SCREEN END  OF BLOCK b1.

*-----------------------------------------------------------------------
* Lógica Principal do Programa
*-----------------------------------------------------------------------
START-OF-SELECTION.

  PERFORM f_seleciona_dados.

*** Se executado como JOB processar o B.I.
  IF sy-batch IS INITIAL.
    PERFORM f_alv.
  ELSE.
    PERFORM f_processa_registros.
  ENDIF.
*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
FORM f_seleciona_dados .

  SELECT * FROM zse01_apropr_seg
    INTO TABLE t_se01
    WHERE chave IN s_chave AND
          bukrs IN s_bukrs.

  IF NOT sy-batch IS INITIAL.
    WRITE:/ 'selecionou:', sy-dbcnt, 'registros' .
  ENDIF.

  IF sy-subrc <> 0.
*    MESSAGE e000(z01) WITH text-m01.
*    STOP.
  ENDIF.

ENDFORM.                    " F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*&      Form  F_ALV
*&---------------------------------------------------------------------*
FORM f_alv .

  PERFORM: f_fieldcat,
           f_modifica_layout,
           f_sort.

  CALL SCREEN 100.

ENDFORM.                    " F_ALV
*&---------------------------------------------------------------------*
*&      Form  F_FIELDCAT
*&---------------------------------------------------------------------*
FORM f_fieldcat .
  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name = 'ZSE01_APROPR_SEG'
    CHANGING
      ct_fieldcat      = it_field.


  READ TABLE it_field INTO w_field INDEX 1.
  w_field-fieldname = 'SEL'.
  w_field-checkbox = 'X'.
  w_field-edit = 'X'.
  w_field-outputlen = '5'.

  MODIFY it_field FROM w_field INDEX 1.
ENDFORM.                    " F_FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  F_MODIFICA_LAYOUT
*&---------------------------------------------------------------------*

FORM f_modifica_layout .


  ref_layout-sel_mode   = ''.
  ref_layout-no_hgridln = ''.
  ref_layout-no_vgridln = ''.
*  ref_layout-no_toolbar = 'X'.
  ref_layout-smalltitle = ''.
  ref_layout-grid_title = text-h02.

ENDFORM.                    " F_MODIFICA_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  F_SORT
*&---------------------------------------------------------------------*
FORM f_sort .
*  it_sort-spos = '1'.
*  it_sort-fieldname = 'AUFNR'.
*  it_sort-up = 'X'.
*  it_sort-group = 'UL'.
**  it_sort-subtot = 'X'.
*  APPEND it_sort TO it_sort1.
*  CLEAR it_sort.
ENDFORM.                    " F_SORT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS '100'.
  SET TITLEBAR '100'.
ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  DATA: v_valid(1) TYPE c,
        lt_rows TYPE lvc_t_row,
         p_rows LIKE lvc_s_row.

  DATA:   v_job_id   LIKE tbtcjob-jobcount,
          v_jobd     LIKE sy-datum,
          v_jobt     LIKE sy-uzeit,
          p_jobn(32),
          p_jobd     LIKE sy-datum,"sy-datum,
          p_jobt     LIKE sy-uzeit." VALUE sy-uzeit.

  p_jobd = sy-datum.
  p_jobt = sy-uzeit.

  CASE sy-ucomm.

    WHEN 'BACK'.
      LEAVE TO SCREEN 0.

    WHEN 'SAVE'.

      CONCATENATE 'ZSEG02' sy-datum sy-uzeit INTO p_jobn SEPARATED BY '-'.
      CALL FUNCTION 'JOB_OPEN'
        EXPORTING
          jobname          = p_jobn
        IMPORTING
          jobcount         = v_job_id
        EXCEPTIONS
          cant_create_job  = 1
          invalid_job_data = 2
          jobname_missing  = 3
          OTHERS           = 4.

      SUBMIT zseg02 WITH s_bukrs IN s_bukrs
                    WITH s_chave IN s_chave
                   VIA JOB p_jobn
                    NUMBER v_job_id
                AND RETURN.


      IF p_jobd  = sy-datum AND p_jobt <= sy-uzeit.
        v_jobd  = ' '.
        v_jobt  = ' '.
*              v_aux_1 = 'X'.
      ELSE.
        v_jobd  = p_jobd.
        v_jobt  = p_jobt.
*              v_aux_1 = ' '.
      ENDIF.

      CALL FUNCTION 'JOB_CLOSE'
        EXPORTING
          jobcount             = v_job_id
          jobname              = p_jobn
          sdlstrtdt            = v_jobd
          sdlstrttm            = v_jobt
          strtimmed            = 'X'
        EXCEPTIONS
          cant_start_immediate = 1
          invalid_startdate    = 2
          jobname_missing      = 3
          job_close_failed     = 4
          job_nosteps          = 5
          job_notex            = 6
          lock_failed          = 7
          OTHERS               = 8.

      LEAVE TO SCREEN 0.
*      PERFORM f_processa_registros.
    WHEN 'DELE'.
      PERFORM f_apaga_registros.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  ALV_EXEC  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE alv_exec OUTPUT.

*  PERFORM z_modifica_table.

* Exclusão de botões da barra de botões standards do objeto ALV
  v_exclu1 = cl_gui_alv_grid=>mc_fc_loc_append_row.
  APPEND v_exclu1 TO v_exclu.

  v_exclu1 = cl_gui_alv_grid=>mc_fc_loc_copy.
  APPEND v_exclu1 TO v_exclu.

  v_exclu1 = cl_gui_alv_grid=>mc_fc_loc_copy_row.
  APPEND v_exclu1 TO v_exclu.

  v_exclu1 = cl_gui_alv_grid=>mc_fc_loc_cut.
  APPEND v_exclu1 TO v_exclu.

  v_exclu1 = cl_gui_alv_grid=>mc_fc_sort_dsc.
  APPEND v_exclu1 TO v_exclu.

  v_exclu1 = cl_gui_alv_grid=>mc_fc_sort_asc.
  APPEND v_exclu1 TO v_exclu.

  v_exclu1 = cl_gui_alv_grid=>mc_fc_loc_delete_row.
  APPEND v_exclu1 TO v_exclu.

  v_exclu1 = cl_gui_alv_grid=>mc_fc_loc_insert_row.
  APPEND v_exclu1 TO v_exclu.

  v_exclu1 = cl_gui_alv_grid=>mc_fc_loc_move_row.
  APPEND v_exclu1 TO v_exclu.

  v_exclu1 = cl_gui_alv_grid=>mc_fc_loc_paste.
  APPEND v_exclu1 TO v_exclu.

  v_exclu1 = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
  APPEND v_exclu1 TO v_exclu.

  v_exclu1 = cl_gui_alv_grid=>mc_fc_loc_undo.
  APPEND v_exclu1 TO v_exclu.

  v_exclu1 = cl_gui_alv_grid=>mc_fc_filter.
  APPEND v_exclu1 TO v_exclu.

  v_exclu1 = cl_gui_alv_grid=>mc_fc_sum.
  APPEND v_exclu1 TO v_exclu.

  v_exclu1 = cl_gui_alv_grid=>mc_fc_subtot.
  APPEND v_exclu1 TO v_exclu.

  v_exclu1 = cl_gui_alv_grid=>mc_fc_save_variant.
  APPEND v_exclu1 TO v_exclu.

  v_exclu1 = cl_gui_alv_grid=>mc_fc_refresh.
  APPEND v_exclu1 TO v_exclu.

  v_exclu1 = cl_gui_alv_grid=>mc_fc_to_office.
  APPEND v_exclu1 TO v_exclu.

  v_exclu1 = cl_gui_alv_grid=>mc_fc_views.
  APPEND v_exclu1 TO v_exclu.

  v_exclu1 = cl_gui_alv_grid=>mc_fc_print.
  APPEND v_exclu1 TO v_exclu.

*  v_exclu1 = cl_gui_alv_grid=>mt_alv_graphics.
*  APPEND v_exclu1 TO v_exclu.



  v_repid = sy-repid.
  IF ref_container IS INITIAL.

    CREATE OBJECT ref_container
      EXPORTING
        repid     = v_repid
        dynnr     = '100'
        extension = 1170.
    IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

    CREATE OBJECT ref_alv
      EXPORTING
        i_parent = ref_container.
    IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
*    IF NOT t_saida_aux[] IS INITIAL.
*      t_saida[] = t_saida_aux[].
*    ENDIF.

    CALL METHOD ref_alv->set_table_for_first_display
      EXPORTING
        is_layout            = ref_layout
        it_toolbar_excluding = v_exclu
      CHANGING
        it_outtab            = t_se01[]
        it_fieldcatalog      = it_field
        it_sort              = it_sort1.
    IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

*   Registra Enter como evento
    CALL METHOD ref_alv->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

*    SET HANDLER:
*                 lcl_event_handler=>handle_data_changed_finished FOR ref_alv.
  ELSE.
    CALL METHOD ref_alv->refresh_table_display.
    CALL METHOD cl_gui_cfw=>flush.
  ENDIF.

ENDMODULE.                 " ALV_EXEC  OUTPUT
*-----------------------------------------------------------------------
*       Form  z_modifica_table
*-----------------------------------------------------------------------
* Método de atualização de tabela interna
*-----------------------------------------------------------------------
FORM z_modifica_table.

* Atualiza Tabela interna conforme dados alimentados em tela
  CALL METHOD ref_alv->check_changed_data
    IMPORTING
      e_valid = v_valid.
ENDFORM.                    " z_modifica_table
*&---------------------------------------------------------------------*
*&      Form  F_APAGA_REGISTROS
*&---------------------------------------------------------------------*
FORM f_apaga_registros .

  DATA: v_answer.

  CALL METHOD ref_alv->get_selected_rows
    IMPORTING
      et_index_rows = lt_rows.

*  IF NOT lt_rows IS INITIAL.
  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar              = text-050
      text_question         = text-051
      text_button_1         = text-052
      icon_button_1         = 'ICON_OKAY'
      text_button_2         = text-053
      icon_button_2         = 'ICON_CANCEL'
      default_button        = '1'
      display_cancel_button = ' '
    IMPORTING
      answer                = v_answer.
  IF v_answer NE '1'.
    EXIT.
  ELSE.
*      LOOP AT lt_rows INTO p_rows.
*
*        IF p_rows-index < 1.
*          CONTINUE.
*        ENDIF.
*
*        READ TABLE t_se01 INDEX p_rows-index.
*        t_se01-sel = 'X'.
*        MODIFY t_se01 INDEX p_rows-index.
    LOOP AT t_se01.
      DELETE FROM zse01_apropr_seg WHERE chave   = t_se01-chave AND
                                         zuonr_d = t_se01-zuonr_d.
    ENDLOOP.
*      ENDLOOP.
    REFRESH t_se01 .
    CLEAR t_se01.
    COMMIT WORK AND WAIT.
  ENDIF.

*  ELSE.
*    MESSAGE i398(00) WITH 'Selecionar pelo menos um registro'.
*  ENDIF.
ENDFORM.                    " F_APAGA_REGISTROS
*&---------------------------------------------------------------------*
*&      Form  F_PROCESSA_REGISTROS
*&---------------------------------------------------------------------*
FORM f_processa_registros .

  DATA: w_data1(10),
        w_data2(10),
        w_data3(10),
        w_data4(10),
        w_valor1(12),
        w_valor2(12),
        w_x1(1),
        w_x2,
      v_msg LIKE sy-lisel,
      v_message_id LIKE  sy-msgid,
      v_message_number LIKE  sy-msgno,
      v_message_var1   LIKE  sy-msgv1,
      v_message_var2   LIKE  sy-msgv2,
      v_message_var3   LIKE  sy-msgv3,
      v_message_var4   LIKE  sy-msgv4.

  DATA: opt TYPE ctu_params.
  opt-dismode = 'N'.
  opt-defsize = 'X'.
  opt-nobinpt = 'X'.
  opt-nobiend = 'X'.
  opt-updmode = 'S'.


*  CALL FUNCTION 'BDC_OPEN_GROUP'
*    EXPORTING
*      client              = sy-mandt
*      group               = p_pasta
*      user                = sy-uname
*    EXCEPTIONS
*      client_invalid      = 1
*      destination_invalid = 2
*      group_invalid       = 3
*      group_is_locked     = 4
*      holddate_invalid    = 5
*      internal_error      = 6
*      queue_error         = 7
*      running             = 8
*      system_lock_error   = 9
*      user_invalid        = 10
*      OTHERS              = 11.
*  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*  ENDIF.




  LOOP AT t_se01.
    CLEAR: bdcdata, messtab.
    REFRESH: bdcdata, messtab.

    WRITE: t_se01-dbbdt   TO w_data1,
           t_se01-dbedt   TO w_data2,
           t_se01-wwert   TO w_data3,
           t_se01-wrbtr_d TO w_valor1,
           t_se01-wrbtr_c TO w_valor2.

    IF NOT t_se01-xfwhw IS INITIAL.
      w_x1 = 'X'.
    ELSE.
      CLEAR w_x1.
    ENDIF.

    IF NOT t_se01-xbllt IS INITIAL.
      w_x2 = 'X'.
    ELSE.
      CLEAR w_x1.
    ENDIF.

    PERFORM bdc_dynpro      USING 'SAPMF05A' '0106'.
    PERFORM bdc_field       USING 'BDC_CURSOR'
                                  'RF05A-NEWKO'.
    PERFORM bdc_field       USING 'BDC_OKCODE'
                                  '/00'.
    PERFORM bdc_field       USING 'BKPF-BUKRS'
                                  t_se01-bukrs.
    PERFORM bdc_field       USING 'BKDF-DBBDT'
                                  w_data1.
    PERFORM bdc_field       USING 'BKDF-DBEDT'
                                  w_data2.
    PERFORM bdc_field       USING 'BKDF-DBMON'
                                  t_se01-dbmon.
    PERFORM bdc_field       USING 'BKDF-DBTAG'
                                  t_se01-dbtag.
    PERFORM bdc_field       USING 'BKDF-XFWHW'
                                  w_x1."t_se01-xfwhw.
    PERFORM bdc_field       USING 'BKDF-XBLLT'
                                  w_x2."t_se01-xbllt.
    PERFORM bdc_field       USING 'BKPF-BLART'
                                  t_se01-blart.
    PERFORM bdc_field       USING 'BKPF-WAERS'
                                  t_se01-waers.
    PERFORM bdc_field       USING 'BKPF-XBLNR'
                                  t_se01-xblnr.
    PERFORM bdc_field       USING 'BKPF-WWERT'
                                  w_data3.
    PERFORM bdc_field       USING 'BKPF-BKTXT'
                                  t_se01-bktxt.
    PERFORM bdc_field       USING 'RF05A-NEWBS'
                                  t_se01-newbs_d.
    PERFORM bdc_field       USING 'RF05A-NEWKO'
                                  t_se01-newko_d.


    PERFORM bdc_dynpro      USING 'SAPMF05A' '0300'.
    PERFORM bdc_field       USING 'BDC_CURSOR'
                                  'RF05A-NEWKO'.
    PERFORM bdc_field       USING 'BDC_OKCODE'
                                  '/00'.
    PERFORM bdc_field       USING 'BSEG-WRBTR'
                                  w_valor1.
    PERFORM bdc_field       USING 'BSEG-BUPLA'
                                  t_se01-bupla_d.
    PERFORM bdc_field       USING 'BSEG-ZUONR'
                                  t_se01-zuonr_d.
    PERFORM bdc_field       USING 'BSEG-SGTXT'
                                  t_se01-sgtxt_d.
    PERFORM bdc_field       USING 'RF05A-NEWBS'
                                  t_se01-newbs_c.
    PERFORM bdc_field       USING 'RF05A-NEWKO'
                                  t_se01-newko_c.
    PERFORM bdc_field       USING 'COBL-KOSTL'
                                  t_se01-kostl_d.
    PERFORM bdc_field       USING 'COBL-AUFNR'
                                  t_se01-aufnr_d.
    PERFORM bdc_field       USING 'COBL-GSBER'
                                  t_se01-gsber_d.


    PERFORM bdc_dynpro      USING 'SAPMF05A' '0300'.
    PERFORM bdc_field       USING 'BDC_CURSOR'
                                  'BSEG-SGTXT'.
    PERFORM bdc_field       USING 'BDC_OKCODE'
                                  '/00'.
    PERFORM bdc_field       USING 'BSEG-WRBTR'
                                  w_valor2.
    PERFORM bdc_field       USING 'BSEG-BUPLA'
                                  t_se01-bupla_c.
    PERFORM bdc_field       USING 'BSEG-ZUONR'
                                  t_se01-zuonr_c.
    PERFORM bdc_field       USING 'BSEG-SGTXT'
                                  t_se01-sgtxt_c.
    PERFORM bdc_field       USING 'COBL-GSBER'
                                  t_se01-gsber_c.


    PERFORM bdc_dynpro      USING 'SAPMF05A' '0300'.
    PERFORM bdc_field       USING 'BDC_CURSOR'
                                  'BSEG-WRBTR'.
    PERFORM bdc_field       USING 'BDC_OKCODE'
                                  '=BU'.
*  PERFORM bdc_field       USING 'BSEG-WRBTR'
*                                t_se01-wrbtr_029.
*  PERFORM bdc_field       USING 'BSEG-BUPLA'
*                                t_se01-bupla_030.
*  PERFORM bdc_field       USING 'BSEG-ZUONR'
*                                t_se01-zuonr_031.
*  PERFORM bdc_field       USING 'BSEG-SGTXT'
*                                t_se01-sgtxt_032.
*  PERFORM bdc_field       USING 'COBL-GSBER'
*                                t_se01-gsber_033.
*    DATA: v_mode(1) VALUE 'A'.

    CALL TRANSACTION 'FBD1'
            USING bdcdata
            OPTIONS FROM opt
            MESSAGES INTO messtab.

*    CALL FUNCTION 'BDC_INSERT'
*     EXPORTING
*       tcode                  = 'FBD1'
**   CTUPARAMS              = ' '
*      TABLES
*        dynprotab              = bdcdata
*     EXCEPTIONS
*       internal_error         = 1
*       not_open               = 2
*       queue_error            = 3
*       tcode_invalid          = 4
*       printing_invalid       = 5
*       posting_invalid        = 6
*       OTHERS                 = 7.

    IF sy-subrc = 0.
      DELETE t_se01.
      DELETE FROM zse01_apropr_seg WHERE chave = t_se01-chave AND
                                         zuonr_d = t_se01-zuonr_d.
    ELSE.
      WRITE:/ 'Erro no processamento FBD1'.


      LOOP AT messtab.
        IF messtab-msgtyp EQ 'E'.
*       it_msg-msgtyp eq 'S'.

          MOVE: messtab-msgid TO v_message_id,
                messtab-msgnr TO v_message_number,
                messtab-msgv1 TO v_message_var1,
                messtab-msgv2 TO v_message_var2,
                messtab-msgv3 TO v_message_var3,
                messtab-msgv4 TO v_message_var4.

          CALL FUNCTION 'RPY_MESSAGE_COMPOSE'
            EXPORTING
              message_id        = v_message_id
              message_number    = v_message_number
              message_var1      = v_message_var1
              message_var2      = v_message_var2
              message_var3      = v_message_var3
              message_var4      = v_message_var4
            IMPORTING
              message_text      = v_msg
            EXCEPTIONS
              message_not_found = 1
              OTHERS            = 2.

          WRITE:/ t_se01-chave, t_se01-zuonr_d, '-->', v_msg.
          skip 1.
*          WRITE:/ .
        ENDIF.
      ENDLOOP.
    ENDIF.

  ENDLOOP.

*  CALL FUNCTION 'BDC_CLOSE_GROUP'
* EXCEPTIONS
*   NOT_OPEN          = 1
*   QUEUE_ERROR       = 2
*   OTHERS            = 3
*          .
*  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*  ENDIF.

*perform bdc_transaction using 'FBD1'.
  COMMIT WORK AND WAIT.
ENDFORM.                    " F_PROCESSA_REGISTROS
*----------------------------------------------------------------------*
*        Start new screen                                              *
*----------------------------------------------------------------------*
FORM bdc_dynpro USING program dynpro.
  CLEAR bdcdata.
  bdcdata-program  = program.
  bdcdata-dynpro   = dynpro.
  bdcdata-dynbegin = 'X'.
  APPEND bdcdata.
ENDFORM.                    "BDC_DYNPRO

*----------------------------------------------------------------------*
*        Insert field                                                  *
*----------------------------------------------------------------------*
FORM bdc_field USING fnam fval.
  CLEAR bdcdata.
  bdcdata-fnam = fnam.
  bdcdata-fval = fval.
  APPEND bdcdata.
ENDFORM.                    "BDC_FIELD

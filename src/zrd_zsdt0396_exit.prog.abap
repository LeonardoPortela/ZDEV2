*&---------------------------------------------------------------------*
*& Report zrd_zsdt0396_exit
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zrd_zsdt0396_exit.


*&---------------------------------------------------------------------*
*&  "Set atributos iniciais ao incluir um novo registro
*&---------------------------------------------------------------------*
FORM f_exit_zsdt0396_0001 CHANGING p_registro_manter TYPE any.

  DATA: wl_zsdt0396 TYPE zsdt0396.

  CLEAR: wl_zsdt0396.

*  wl_zsdt0396-user_create = sy-uname.
*  wl_zsdt0396-date_create = sy-datum.
*  wl_zsdt0396-time_create = sy-uzeit.

  MOVE-CORRESPONDING wl_zsdt0396 TO p_registro_manter.

ENDFORM.

*&---------------------------------------------------------------------*
*&    "Validações antes da gravação do registro
*&---------------------------------------------------------------------*
FORM f_exit_zsdt0396_0002 USING p_registro_manter TYPE any
                       CHANGING p_erro.

* Declaração de Estruturas
  DATA: w_zsdt0396 TYPE zsdt0396.
  CLEAR: w_zsdt0396.

  MOVE-CORRESPONDING p_registro_manter TO w_zsdt0396.

  " Verificar campo setor_ativ preenchido
  IF w_zsdt0396-setor_ativ IS INITIAL.
    p_erro = abap_true.
    MESSAGE s024(sd) WITH 'Informar Código do Setor de Atividade.'
                     DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  " Verificar campo regional preenchido
  IF w_zsdt0396-regional IS INITIAL.
    p_erro = abap_true.
    MESSAGE s024(sd) WITH 'Informar Código da Regional.'
                     DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  " Validar se regional existe
  SELECT SINGLE *
    FROM zsdt0270
    INTO @DATA(w_zsdt0270)
    WHERE cod_regional = @w_zsdt0396-regional.

  IF sy-subrc <> 0.
    p_erro = abap_true.
    MESSAGE s024(sd) WITH 'Regional informada está incorreta.'
                     DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  " Validar se setor de atividade existe
  SELECT SINGLE *
    FROM tspat
    INTO @DATA(w_setor_ativ)
    WHERE spras = 'P'
      AND spart = @w_zsdt0396-setor_ativ.

  IF sy-subrc <> 0.
    p_erro = abap_true.
    MESSAGE s024(sd) WITH 'Setor de Atividade informado está incorreto.'
                     DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  MOVE-CORRESPONDING p_registro_manter TO w_zsdt0396.

ENDFORM.


*&---------------------------------------------------------------------*
*&    "Set atributos antes de gravar o registro
*&---------------------------------------------------------------------*
FORM f_exit_zsdt0396_0003 CHANGING p_registro_manter TYPE any.

  DATA: wl_zsdt0396 TYPE zsdt0396.

  CLEAR: wl_zsdt0396.

  MOVE-CORRESPONDING p_registro_manter TO wl_zsdt0396.
**<<<------"180709 - NMS - INI------>>>
  SELECT SINGLE * FROM zsdt0396 INTO @DATA(el_t0396) WHERE regional   EQ @wl_zsdt0396-regional
                                                       AND setor_ativ EQ @wl_zsdt0396-setor_ativ
                                                       AND user_aprov EQ @wl_zsdt0396-user_aprov
                                                       AND cancel     EQ @space.

  IF sy-subrc IS INITIAL.
    MESSAGE |Registro já existe na base. Registro Duplicado.| TYPE 'E'.

  ENDIF.
**<<<------"180709 - NMS - FIM------>>>
  DATA(lv_date) = sy-datum.
  DATA(lv_time) = sy-uzeit.

  CALL FUNCTION 'CONVERT_INTO_TIMESTAMP'
    EXPORTING
      i_datlo     = lv_date
      i_timlo     = lv_time
      i_tzone     = sy-zonlo
    IMPORTING
      e_timestamp = wl_zsdt0396-timestamp.

  wl_zsdt0396-mandt = sy-mandt.
  wl_zsdt0396-date_create = sy-datum.
  wl_zsdt0396-time_create = sy-uzeit.
  wl_zsdt0396-user_create = sy-uname.

  MOVE-CORRESPONDING   wl_zsdt0396 TO p_registro_manter.


  INSERT zsdt0396 FROM wl_zsdt0396.  "Passei o campo p_nosave no parametro da transação ZSDT0353 para controlar o delete e com isso o registro não é inserido automaticamente.
  IF sy-subrc = 0.
    COMMIT WORK.
  ENDIF.


ENDFORM.


FORM f_exit_zsdt0396_0004 CHANGING p_registro_manter TYPE any.

  DATA: wl_zsdt0396 TYPE zsdt0396.

  CLEAR wl_zsdt0396.

  MOVE-CORRESPONDING p_registro_manter  TO wl_zsdt0396.

  SELECT SINGLE *
           FROM zsdt0270
           INTO @DATA(w_zsdt0270)
          WHERE cod_regional = @wl_zsdt0396-regional.
  IF sy-subrc = 0.
    wl_zsdt0396-desc_regional = w_zsdt0270-regional.
  ELSE.
    CLEAR wl_zsdt0396-desc_regional.
  ENDIF.


  SELECT SINGLE *
           FROM tspat
           INTO @DATA(w_setor_ativ)
          WHERE spras = 'P'
            AND spart = @wl_zsdt0396-setor_ativ.

  IF sy-subrc = 0.
    wl_zsdt0396-desc_setor_ativ = w_setor_ativ-vtext.
  ELSE.
    CLEAR wl_zsdt0396-desc_setor_ativ.
  ENDIF.


  IF wl_zsdt0396-cancel = abap_true.
    CLEAR wl_zsdt0396.
  ENDIF.


  MOVE-CORRESPONDING wl_zsdt0396 TO p_registro_manter.

ENDFORM.

FORM f_exit_zsdt0396_0005 CHANGING p_registro_manter TYPE any.

  FIELD-SYMBOLS: <fs_name1> TYPE any,
                 <fs_name2> TYPE any.

  DATA: w_zsdt0396 TYPE zsdt0396_out.

  CLEAR w_zsdt0396.

  ASSIGN ('(ZREGISTER_DATA)DD03L-PRECFIELD') TO <fs_name1>. "Regional
  IF sy-subrc = 0.
    CLEAR <fs_name1>.
  ENDIF.

  ASSIGN ('(ZREGISTER_DATA)DD03L-REFFIELD') TO <fs_name2>. "Setor de atividade
  IF sy-subrc = 0.
    CLEAR <fs_name2>.
  ENDIF.

  MOVE-CORRESPONDING p_registro_manter  TO w_zsdt0396.

  SELECT SINGLE *
           FROM zsdt0270
           INTO @DATA(w_zsdt0270)
          WHERE cod_regional = @w_zsdt0396-regional.
  IF sy-subrc = 0.
    ASSIGN ('(ZREGISTER_DATA)DD03L-PRECFIELD') TO <fs_name1>.

    IF sy-subrc = 0.
      <fs_name1> = w_zsdt0270-regional.
    ENDIF.


  ENDIF.

  SELECT SINGLE *
           FROM tspat
           INTO @DATA(w_setor_ativ)
          WHERE spras = 'P'
            AND spart = @w_zsdt0396-setor_ativ.

  IF sy-subrc = 0.
    ASSIGN ('(ZREGISTER_DATA)DD03L-REFFIELD') TO <fs_name2>.
    IF sy-subrc = 0.
      <fs_name2> =  w_setor_ativ-vtext.
    ENDIF.
  ENDIF.

ENDFORM.



FORM f_exit_zsdt0396_0006 USING p_saida TYPE any
                          CHANGING p_error.
  "Validações antes de apagar.

  DATA: wl_zsdt0396 TYPE zsdt0396,
        t_zsdt0396  TYPE TABLE OF zsdt0396.

  CLEAR wl_zsdt0396.

  "Passei o campo p_nosave no parametro da transação ZSDT0353 e com isso o registro não é excluido do banco.

  MOVE-CORRESPONDING p_saida  TO wl_zsdt0396.

  UPDATE zsdt0396
    SET cancel      = abap_true
        date_cancel = sy-datum
        time_cancel = sy-uzeit
        user_cancel = sy-uname
    WHERE timestamp =  wl_zsdt0396-timestamp
      AND regional = wl_zsdt0396-regional
      AND setor_ativ = wl_zsdt0396-setor_ativ.

  IF sy-subrc = 0.
    COMMIT WORK.
  ENDIF.

ENDFORM.

FORM f_exit_zsdt0396_0008 CHANGING p_col_pos
                                  p_ref_tabname
                                  p_ref_fieldname
                                  p_tabname
                                  p_field
                                  p_scrtext_l
                                  p_outputlen
                                  p_edit
                                  p_sum
                                  p_emphasize
                                  p_just
                                  p_hotspot
                                  p_f4
                                  p_check.


ENDFORM.
**<<<------"180709 - NMS - INI------>>>
*Configuração do Catalogo de Campos do ALV.
FORM f_exit_zsdt0396_0008_v2 CHANGING ct_fcat_out TYPE lvc_s_fcat.

* Verifica o nome do campo.
  CASE ct_fcat_out-fieldname.
    WHEN 'TIMESTAMP'.
      ct_fcat_out-no_out = abap_on. "Oculta no relatório
      ct_fcat_out-tech   = abap_on. "Elinina da cesta de seleção da modificação do layout.

    WHEN OTHERS.
* Do nothing
  ENDCASE.

ENDFORM.
**<<<------"180709 - NMS - FIM------>>>
FORM  f_exit_zsdt0396_0009 TABLES pt_excl_toolbar
                           USING p_db_tab.
*
  TYPES: BEGIN OF ty_excl_toolbar,
           code TYPE ui_func.
  TYPES: END OF ty_excl_toolbar.

  DATA: it_excl_toolbar TYPE TABLE OF ty_excl_toolbar,
        wa_excl_toolbar TYPE ty_excl_toolbar.

  FREE: it_excl_toolbar.


  wa_excl_toolbar-code = 'Modificar'.
  APPEND wa_excl_toolbar  TO it_excl_toolbar.

  pt_excl_toolbar[] = it_excl_toolbar[].
ENDFORM.

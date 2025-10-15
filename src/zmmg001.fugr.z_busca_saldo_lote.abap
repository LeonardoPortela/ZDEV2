FUNCTION z_busca_saldo_lote.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_MATNR) TYPE  MATNR_D OPTIONAL
*"     REFERENCE(I_DATUM) TYPE  DATUM
*"  TABLES
*"      T_ZMM_SALDO_LOTE STRUCTURE  ZMM_SALDO_LOTE
*"----------------------------------------------------------------------

  TYPES: BEGIN OF type_j_1bbranch,
           bukrs  TYPE j_1bbranch-bukrs,
           branch TYPE j_1bbranch-branch,
         END OF type_j_1bbranch.

  DATA  : it_selection  TYPE TABLE OF rsparams,
          it_selelgort  TYPE TABLE OF rsparams,
          wa_selection  LIKE LINE OF it_selection,
          wa_selelgort  LIKE LINE OF it_selection,
          it_mchb       TYPE TABLE OF mchb,
          wa_j_1bbranch TYPE type_j_1bbranch,
          xdep(1),
          xlote(1).

  DATA: so_data TYPE RANGE OF mkpf-budat,
        wa_data LIKE LINE OF so_data.

  REFRESH it_mchb.
  CLEAR: xdep, xlote.

  LOOP AT t_zmm_saldo_lote.
    IF t_zmm_saldo_lote-lgort IS NOT INITIAL.
      xdep = 'X'.
      EXIT.
    ENDIF.
  ENDLOOP.

  LOOP AT t_zmm_saldo_lote.
    IF t_zmm_saldo_lote-charg IS NOT INITIAL.
      xlote = 'X'.
      EXIT.
    ENDIF.
  ENDLOOP.

*  IF  xdep = '' AND  xlote = ''.
*    SELECT  *
*        INTO TABLE it_mchb
*        FROM mchb
*        FOR ALL ENTRIES IN t_zmm_saldo_lote
*        WHERE matnr = t_zmm_saldo_lote-matnr
*        AND   werks = t_zmm_saldo_lote-werks.
*  ELSEIF  xdep = 'X' AND  xlote = ''.
*    SELECT  *
*        INTO TABLE it_mchb
*        FROM mchb
*        FOR ALL ENTRIES IN t_zmm_saldo_lote
*        WHERE matnr = t_zmm_saldo_lote-matnr
*        AND   werks = t_zmm_saldo_lote-werks
*        AND   lgort = t_zmm_saldo_lote-lgort.
*  ELSEIF  xdep = ' ' AND  xlote = 'X'.
*    SELECT  *
*        INTO TABLE it_mchb
*        FROM mchb
*        FOR ALL ENTRIES IN t_zmm_saldo_lote
*        WHERE matnr = t_zmm_saldo_lote-matnr
*        AND   werks = t_zmm_saldo_lote-werks
*        AND   charg = t_zmm_saldo_lote-charg.
*  ELSE.
*    SELECT  *
*        INTO TABLE it_mchb
*        FROM mchb
*        FOR ALL ENTRIES IN t_zmm_saldo_lote
*        WHERE matnr = t_zmm_saldo_lote-matnr
*        AND   werks = t_zmm_saldo_lote-werks
*        AND   lgort = t_zmm_saldo_lote-lgort
*        AND   charg = t_zmm_saldo_lote-charg.
*  ENDIF.
*
*  IF it_mchb[] IS INITIAL.
*    RETURN.
*  ENDIF.

  READ TABLE t_zmm_saldo_lote INDEX 1.
  SELECT SINGLE bukrs branch
     FROM j_1bbranch
     INTO wa_j_1bbranch
    WHERE branch EQ t_zmm_saldo_lote-werks.

  IF t_zmm_saldo_lote-werks(2) EQ 'F1'. " RJF - CS2023000364 Inclusao de regra e botao para estorno

    wa_selection-selname = 'BUKRS'.
    wa_selection-kind    = 'S'. "S-Select-options P-Parameters
    wa_selection-sign    = 'I'.
    wa_selection-option  = 'EQ'.
    wa_selection-low     = '0100'.
    APPEND wa_selection TO it_selection.

  ELSE.
    wa_selection-selname = 'BUKRS'.
    wa_selection-kind    = 'S'. "S-Select-options P-Parameters
    wa_selection-sign    = 'I'.
    wa_selection-option  = 'EQ'.
    wa_selection-low     = wa_j_1bbranch-bukrs.
    APPEND wa_selection TO it_selection.
  ENDIF.

  LOOP AT t_zmm_saldo_lote.
    IF t_zmm_saldo_lote-werks IS NOT INITIAL.
      wa_selection-selname = 'WERKS'.
      wa_selection-kind    = 'S'. "S-Select-options P-Parameters
      wa_selection-sign    = 'I'.
      wa_selection-option  = 'EQ'.
      wa_selection-low     = t_zmm_saldo_lote-werks.
      APPEND wa_selection TO it_selection.
    ENDIF.
  ENDLOOP.

  LOOP AT t_zmm_saldo_lote.
    IF t_zmm_saldo_lote-matnr IS NOT INITIAL.
      wa_selection-selname = 'MATNR'.
      wa_selection-kind    = 'S'. "S-Select-options P-Parameters
      wa_selection-sign    = 'I'.
      wa_selection-option  = 'EQ'.
      wa_selection-low     = t_zmm_saldo_lote-matnr.
      APPEND wa_selection TO it_selection.
    ENDIF.
  ENDLOOP.

*  wa_selection-selname = 'MATNR'.
*  wa_selection-kind    = 'S'. "S-Select-options P-Parameters
*  wa_selection-sign    = 'I'.
*  wa_selection-option  = 'EQ'.
*  wa_selection-low     = i_matnr.
*  APPEND wa_selection TO it_selection.


  LOOP AT t_zmm_saldo_lote.
    IF t_zmm_saldo_lote-charg IS NOT INITIAL.
      wa_selection-selname = 'CHARG'.
      wa_selection-kind    = 'S'. "S-Select-options P-Parameters
      wa_selection-sign    = 'I'.
      wa_selection-option  = 'EQ'.
      wa_selection-low     = t_zmm_saldo_lote-charg.
      APPEND wa_selection TO it_selection.
    ENDIF.
  ENDLOOP.

  LOOP AT t_zmm_saldo_lote.
    IF t_zmm_saldo_lote-lgort IS NOT INITIAL.
      wa_selection-selname = 'LGORT'.
      wa_selection-kind    = 'S'. "S-Select-options P-Parameters
      wa_selection-sign    = 'I'.
      wa_selection-option  = 'EQ'.
      wa_selection-low     = t_zmm_saldo_lote-lgort.
      APPEND wa_selection TO it_selection.
    ENDIF.
  ENDLOOP.

*  WA_SELECTION-SELNAME = 'DATUM'.
*  WA_SELECTION-KIND    = 'S'. "S-Select-options P-Parameters
*  WA_SELECTION-SIGN    = 'I'.
*  WA_SELECTION-OPTION  = 'EQ'.
*  WA_SELECTION-LOW     = I_DATUM.
*  APPEND WA_SELECTION TO IT_SELECTION.


  wa_data-sign = 'I'.
  wa_data-option = 'EQ'.
  wa_data-low = i_datum.
  APPEND wa_data  TO so_data.

  FIELD-SYMBOLS: <lt_data>      TYPE ANY TABLE,
                 <lt_data_line> TYPE ANY TABLE,
                 <ls_data>      TYPE any,
                 <ls_data_line> TYPE any.


  DATA: lr_data            TYPE REF TO data,
        lr_data_line       TYPE REF TO data,
        lr_data_descr      TYPE REF TO cl_abap_datadescr,
        lr_data_line_descr TYPE REF TO cl_abap_datadescr.

  cl_salv_bs_runtime_info=>set(
    EXPORTING display  = abap_false
              metadata = abap_false
              data     = abap_true ).


  REFRESH t_zmm_saldo_lote.

  SUBMIT zrm07mlbd_2 WITH SELECTION-TABLE it_selection
                  WITH datum    IN so_data
                  WITH lgbst    = 'X'
                  WITH bwbst    = ' '
                  WITH sbbst    = ' '
                  WITH xsum     = ' '
                  WITH xchar    = 'X'
                  WITH p_xlgort = 'X'
                  WITH pa_sflva   = '/AR CUSTEIO'
                  WITH pa_sumfl = 'X' AND RETURN.
  TRY.
      cl_salv_bs_runtime_info=>get_data_ref(
          IMPORTING r_data_descr      = lr_data_descr
                    r_data_line_descr = lr_data_line_descr ).

      CREATE DATA lr_data TYPE HANDLE lr_data_descr.
      CREATE DATA lr_data_line TYPE HANDLE lr_data_line_descr.

      ASSIGN lr_data->* TO <lt_data>.
      ASSIGN lr_data_line->* TO <lt_data_line>.

      cl_salv_bs_runtime_info=>get_data(
        IMPORTING t_data      = <lt_data>
                  t_data_line = <lt_data_line> ).

    CATCH cx_salv_bs_sc_runtime_info.
*      MESSAGE 'Não é possível recuperar os dados ALV' TYPE 'E'.
  ENDTRY.

  cl_salv_bs_runtime_info=>clear_all( ).

  ASSIGN lr_data->* TO <ls_data>.

  LOOP AT <lt_data> ASSIGNING <ls_data>.
    MOVE-CORRESPONDING <ls_data> TO t_zmm_saldo_lote.
    t_zmm_saldo_lote-menge = t_zmm_saldo_lote-ENDMENGE.
    APPEND t_zmm_saldo_lote.
  ENDLOOP.

*    ASSIGN lr_data_line->* TO <ls_data_line>.
*  LOOP AT <lt_data_line> ASSIGNING <ls_data_line>.
*    MOVE-CORRESPONDING <ls_data_line> TO t_zmm_saldo_lote.
*    APPEND t_zmm_saldo_lote.
*  ENDLOOP.
**  DELETE t_zmm_saldo_lote WHERE counter NE 4.
  FREE: <lt_data>, <lt_data_line>.
  FREE: lr_data, lr_data_line.
*  ENDLOOP.
  "




ENDFUNCTION.

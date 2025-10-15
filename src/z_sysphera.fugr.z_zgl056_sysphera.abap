FUNCTION z_zgl056_sysphera.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_BUKRS) TYPE  BUKRS
*"     REFERENCE(I_ANO) TYPE  GJAHR
*"     REFERENCE(I_MES) TYPE  MONAT
*"  TABLES
*"      RESULTADO STRUCTURE  ZSYS_ZGL056
*"----------------------------------------------------------------------

  FIELD-SYMBOLS: <lt_data>      TYPE ANY TABLE,
                 <lt_data_line> TYPE ANY TABLE,
                 <ls_data>      TYPE any,
                 <ls_data_line> TYPE any.

*-------------------------------------------
* types
*-------------------------------------------
  TYPES: BEGIN OF ty_objnr,
           objnr_n1 TYPE objnr_n1,
           wogbtr   TYPE wogxxx,
           twaer    TYPE twaer,
         END OF ty_objnr.

*-------------------------------------------
* variaveis locais
*-------------------------------------------
  DATA: so_data            TYPE RANGE OF mkpf-budat,
        wa_data            LIKE LINE OF so_data,
        so_bukrs           TYPE RANGE OF t001-bukrs,
        wa_bukrs           LIKE LINE OF so_bukrs,
        vdatai             TYPE sy-datum,
        w_resultado        TYPE zsys_zgl056,
        v_variante         TYPE slis_vari,
        v_objnr            TYPE ty_objnr,

        lr_data            TYPE REF TO data,
        lr_data_line       TYPE REF TO data,
        lr_data_descr      TYPE REF TO cl_abap_datadescr,
        lr_data_line_descr TYPE REF TO cl_abap_datadescr.

  FREE: resultado,
        so_bukrs, wa_bukrs,
        so_data,  wa_data.

*-------------------------------------------
* init alv
*-------------------------------------------
  cl_salv_bs_runtime_info=>set(
    EXPORTING display  = abap_false
              metadata = abap_false
              data     = abap_true ).

*-------------------------------------------
* parametro empresa
*-------------------------------------------
  wa_bukrs-sign     = 'I'.
  wa_bukrs-option   = 'EQ'.
  wa_bukrs-low      = i_bukrs.
  APPEND wa_bukrs  TO so_bukrs.

*-------------------------------------------
* parametro data
*-------------------------------------------
  vdatai            = |{ i_ano }{ i_mes }01|.

  wa_data-sign      = 'I'.
  wa_data-option    = 'GE'.
  wa_data-low       = vdatai.
  APPEND wa_data   TO so_data.

*-------------------------------------------
* execute ZGL056
*-------------------------------------------
  SUBMIT zgl033 WITH s_bukrs IN so_bukrs
                WITH s_vgate IN so_data
                WITH p_call   = abap_true
                 AND RETURN.

  TRY.
      cl_salv_bs_runtime_info=>get_data_ref(
          IMPORTING r_data_descr      = lr_data_descr
                    r_data_line_descr = lr_data_line_descr ).

      IF lr_data_descr IS NOT INITIAL.
        CREATE DATA lr_data TYPE HANDLE lr_data_descr.
        ASSIGN lr_data->*     TO <lt_data>.
        cl_salv_bs_runtime_info=>get_data( IMPORTING t_data = <lt_data> ).
      ENDIF.

    CATCH cx_salv_bs_sc_runtime_info.
      MESSAGE 'Não é possível recuperar os dados ALV' TYPE 'E'.
  ENDTRY.

  cl_salv_bs_runtime_info=>clear_all( ).

  IF lr_data_descr IS INITIAL.
    EXIT.
  ENDIF.

  ASSIGN lr_data->* TO <ls_data>.

*-------------------------------------------
* processamento
*-------------------------------------------
  LOOP AT <lt_data> ASSIGNING <ls_data>.

    MOVE-CORRESPONDING <ls_data> TO w_resultado.
    MOVE-CORRESPONDING <ls_data> TO v_objnr.

    APPEND w_resultado           TO resultado.
    CLEAR w_resultado.
  ENDLOOP.

  FREE: <lt_data>.
  FREE: lr_data.

ENDFUNCTION.

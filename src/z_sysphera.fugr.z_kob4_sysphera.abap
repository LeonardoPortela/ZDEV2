FUNCTION z_kob4_sysphera.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_AREA) TYPE  KOKRS DEFAULT 'MAGI'
*"     VALUE(IT_ORDENS) TYPE  ZTTSYS_AUFNR
*"  TABLES
*"      RESULTADO STRUCTURE  ZSYS_KOB4
*"----------------------------------------------------------------------

  DATA: lr_gjahr         TYPE RANGE OF gjahr,
        lt_resultado_aux TYPE TABLE OF zsys_kob4,
        lr_aufnr         TYPE RANGE OF aufnr,
        w_resultado      LIKE LINE OF resultado.

  lr_gjahr = VALUE #( sign = 'I' option = 'BT' ( low = '2010' high = '2099' ) ).
  lr_aufnr = VALUE #( FOR ls_ordens IN it_ordens ( sign = 'I' option = 'EQ' low = ls_ordens ) ).

  cl_salv_bs_runtime_info=>set(
    EXPORTING display  = abap_false
              metadata = abap_false
              data     = abap_true ).

  SUBMIT rkaep000 WITH p_tcode  EQ 'KOB4'
                  WITH p_kokrs  EQ i_area
                  with p_maxsel eq 999999999
                  WITH aufnr    IN lr_aufnr
                  WITH r_gjahr2 IN lr_gjahr
                  EXPORTING LIST TO MEMORY
                  AND RETURN.


  TRY.
      cl_salv_bs_runtime_info=>get_data_ref(
          IMPORTING r_data_descr      = lr_data_descr
                    r_data_line_descr = lr_data_line_descr ).

      IF lr_data_descr IS NOT INITIAL.
        CREATE DATA lr_data TYPE HANDLE lr_data_descr.


        ASSIGN lr_data->* TO <lt_data>.

        cl_salv_bs_runtime_info=>get_data(
          IMPORTING t_data      = <lt_data> ).
      ENDIF.

    CATCH cx_salv_bs_sc_runtime_info.
*      MESSAGE 'Não é possível recuperar os dados ALV' TYPE 'E'.
  ENDTRY.

  CHECK <lt_data> IS ASSIGNED.

  LOOP AT <lt_data> ASSIGNING <ls_data>.
    MOVE-CORRESPONDING <ls_data> TO w_resultado.
    IF w_resultado-wtjhr EQ 0 AND
       w_resultado-wljhr EQ 0.
      CONTINUE.
    ENDIF.
    APPEND w_resultado TO lt_resultado_aux.
  ENDLOOP.

  CLEAR w_resultado.
  LOOP AT lt_resultado_aux INTO DATA(w_resultado_aux).
    w_resultado-wtjhr   = w_resultado-wtjhr + w_resultado_aux-wtjhr.
    w_resultado-wljhr   = w_resultado-wljhr + w_resultado_aux-wljhr.
    w_resultado-aufnr   = w_resultado_aux-aufnr  .
    w_resultado-obj_txt = w_resultado_aux-obj_txt.

    AT END OF aufnr.
      APPEND w_resultado TO resultado.
      CLEAR: w_resultado.
    ENDAT.
  ENDLOOP.
ENDFUNCTION.

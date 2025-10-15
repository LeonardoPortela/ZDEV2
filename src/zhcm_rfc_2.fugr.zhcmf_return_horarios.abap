FUNCTION zhcmf_return_horarios.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(WERKS) LIKE  T001P-WERKS OPTIONAL
*"  TABLES
*"      T_SAIDA STRUCTURE  ZHCMS_RETURN_HORARIOS
*"----------------------------------------------------------------------
  DATA: t_rsparams TYPE TABLE OF rsparams.

  TYPES: BEGIN OF ty_result,
           mofid TYPE zhcms_return_horarios-calendario,
           schkz TYPE zhcms_return_horarios-cod_horario,
           rtext TYPE zhcms_return_horarios-desc_horario,
           zmodn TYPE zhcms_return_horarios-cod_h_peri,
           tprog TYPE zhcms_return_horarios-cd_h_diario,
           sobeg TYPE zhcms_return_horarios-inicio_d,
           soend TYPE zhcms_return_horarios-fim_d,
           pamod TYPE zhcms_return_horarios-intervalo,
           pabeg TYPE zhcms_return_horarios-inicio_int,
           paend TYPE zhcms_return_horarios-fim_int,
           tgstd TYPE zhcms_return_horarios-hrstrabdia,
           wostd TYPE zhcms_return_horarios-hrstrabsem,
           m1std TYPE zhcms_return_horarios-hrstrabmes,
           wkwdy TYPE zhcms_return_horarios-diassems,
         END OF ty_result.

  DATA: t_result       TYPE TABLE OF ty_result.

  FIELD-SYMBOLS:<t_data>      TYPE ANY TABLE,
                <t_data_line> TYPE ANY TABLE,
                <w_data>      TYPE any,
                <w_data_line> TYPE any.


  DATA: l_data            TYPE REF TO data,
        l_data_line       TYPE REF TO data,
        l_data_descr      TYPE REF TO cl_abap_datadescr,
        l_data_line_descr TYPE REF TO cl_abap_datadescr.

  DATA: lwa_saida TYPE zhcms_return_horarios.

* 1 -  Seleção das informações filial, nome da filial, calendário tabela T001P.
  SELECT *
    FROM t001p
    INTO TABLE @DATA(t_t001p)
  WHERE btrtl   = '0001' "Subárea
    AND molga  = '37'  "Pais
    AND werks   = @werks.
  "AND mofid   = @mofid.

  IF sy-subrc = 0.

* 2 - Nome do calendário tabela THOCT
    SELECT *
      FROM  thoct
    INTO TABLE @DATA(t_thoct)
      FOR ALL ENTRIES IN @t_t001p
      WHERE ident EQ @t_t001p-mofid
        AND spras = @sy-langu.

* Nome Filial
    SELECT *
    FROM t001w
    INTO TABLE @DATA(t_t001w)
    WHERE  werks   = @werks.
    "AND mofid   = @mofid.


* 3 -  Código do horário.
    SELECT *
      FROM  t508a
    INTO TABLE @DATA(t_t508a)
      FOR ALL ENTRIES IN @t_t001p
      WHERE zeity ='1'
        AND mofid EQ @t_t001p-mofid
        AND mosid = '37'
        AND endda >= @sy-datum.


* 4 -  Plano de horário de trabalho (completo: entrada, intervalo, saída e dias da semana e índices se houver)

    LOOP AT t_t001p INTO DATA(lwa_t001p).

      DATA(w_params) = VALUE rsparams( selname = 'S_MOFID' sign = 'I' kind = 'P' option = 'EQ' low =  lwa_t001p-mofid ).
      APPEND w_params TO t_rsparams[].
      CLEAR: w_params.

      LOOP AT t_t508a INTO DATA(lwa_t508a) WHERE mofid = lwa_t001p-mofid.
        w_params = VALUE #( selname = 'S_SCHKZ' sign = 'I' kind = 'P' option = 'EQ' low = lwa_t508a-schkz ).
        APPEND w_params TO t_rsparams[].
        CLEAR: w_params.
      ENDLOOP.

      w_params = VALUE #( selname = 'R_PERIO' sign = 'I' kind = 'P' option = 'EQ' low = 'X' ).
      APPEND w_params TO t_rsparams[].
      CLEAR: w_params.

    ENDLOOP.


    IF <t_data> IS ASSIGNED.
      CLEAR: <t_data>[].
    ENDIF.

    IF <t_data_line> IS ASSIGNED.
      CLEAR: <t_data_line>[].
    ENDIF.

    IF <t_data> IS ASSIGNED.
      CLEAR: <t_data>.
    ENDIF.

    IF <t_data_line> IS ASSIGNED.
      CLEAR: <t_data_line>.
    ENDIF.

    FREE: l_data,  l_data_line,  l_data_descr,  l_data_line_descr.

    cl_salv_bs_runtime_info=>set( EXPORTING display  = abap_false
                                            metadata = abap_false
                                            data     = abap_true ).


    SUBMIT zhcmr_pt0018 WITH SELECTION-TABLE t_rsparams[] AND RETURN.

    TRY.
        cl_salv_bs_runtime_info=>get_data_ref(
        IMPORTING r_data_descr  = l_data_descr
                  r_data_line_descr = l_data_line_descr ).

        CHECK ( l_data_descr IS NOT INITIAL ) OR ( l_data_line_descr IS  NOT INITIAL ).

        CREATE DATA l_data      TYPE HANDLE  l_data_descr.
        CREATE DATA l_data_line TYPE HANDLE  l_data_line_descr.

        ASSIGN l_data->* TO <t_data>.
        ASSIGN l_data_line->* TO <t_data_line>.

        cl_salv_bs_runtime_info=>get_data( IMPORTING t_data      = <t_data>
                                                     t_data_line = <t_data_line> ).
      CATCH cx_salv_bs_sc_runtime_info.
    ENDTRY.

    cl_salv_bs_runtime_info=>clear_all( ).

    ASSIGN l_data->*        TO <w_data>.
    ASSIGN l_data_line->*   TO <w_data_line>.


    IF <t_data> IS ASSIGNED.

      t_result[] = CORRESPONDING #( <t_data> ).

      LOOP AT t_result INTO DATA(lwa_result).

        lwa_saida-werks = lwa_t001p-werks.  "- filial

        READ TABLE t_t001w INTO DATA(lwa_t001w) WITH KEY werks = lwa_t001p-werks.

        lwa_saida-desc_werks = lwa_t001w-name1.  "- Nome filial


        READ TABLE t_thoct INTO DATA(lwa_thoct) WITH KEY ident = lwa_t001p-mofid.

        lwa_saida-desc_calend = lwa_thoct-ltext.  "- Descrição do calendário.

        lwa_saida-calendario      =    lwa_result-mofid.
        lwa_saida-cod_horario     =    lwa_result-schkz.
        lwa_saida-desc_horario    =    lwa_result-rtext.
        lwa_saida-cod_h_peri      =    lwa_result-zmodn.
        lwa_saida-cd_h_diario     =    lwa_result-tprog.
        lwa_saida-inicio_d        =    lwa_result-sobeg.
        lwa_saida-fim_d           =    lwa_result-soend.
        lwa_saida-intervalo       =    lwa_result-pamod.
        lwa_saida-inicio_int      =    lwa_result-pabeg.
        lwa_saida-fim_int         =    lwa_result-paend.
        lwa_saida-hrstrabdia      =    lwa_result-tgstd.
        lwa_saida-hrstrabsem      =    lwa_result-wostd.
        lwa_saida-hrstrabmes      =    lwa_result-m1std.
        lwa_saida-diassems        =    lwa_result-wkwdy.

        IF lwa_saida-inicio_d IS INITIAL or lwa_saida-inicio_d EQ 000000 .
          lwa_saida-inicio_d = 000000.
        ENDIF.

        IF  lwa_saida-fim_d   IS INITIAL OR lwa_saida-inicio_d EQ 000000.
          lwa_saida-fim_d    =  000000.
        ENDIF.


        APPEND lwa_saida TO t_saida.
        CLEAR: lwa_saida, lwa_result.

      ENDLOOP.

    ENDIF.


    cl_salv_bs_runtime_info=>clear_all( ).

  ENDIF.

ENDFUNCTION.

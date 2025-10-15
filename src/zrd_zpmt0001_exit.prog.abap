*&---------------------------------------------------------------------*
*& Report  ZRD_ZGLT0102_EXIT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zrd_zpmt0001_exit.

FORM f_exit_zpmt0001_0001 CHANGING p_registro_manter TYPE any.

  DATA: wl_zpmt0001 TYPE zpmt0001.

  CLEAR: wl_zpmt0001.

  MOVE-CORRESPONDING p_registro_manter TO wl_zpmt0001.

  IF wl_zpmt0001-criador IS INITIAL.
    wl_zpmt0001-criador   = sy-uname.
    wl_zpmt0001-data        = sy-datum.
    wl_zpmt0001-hora        = sy-uzeit.
  ENDIF.
  wl_zpmt0001-modificador = sy-uname.
  wl_zpmt0001-datam        = sy-datum.
  wl_zpmt0001-horam        = sy-uzeit.

  MOVE-CORRESPONDING wl_zpmt0001 TO p_registro_manter.

ENDFORM.


FORM f_exit_zpmt0001_0002 USING p_registro_manter TYPE any
                       CHANGING p_error.

  TYPES: BEGIN OF ty_cod_oper_cliente,
           low  TYPE tvarvc-low,
           high TYPE tvarvc-high,
         END   OF ty_cod_oper_cliente.

  TYPES: BEGIN OF ty_list_ccusto,
           kostlg TYPE zkostlg,
         END OF ty_list_ccusto.

  DATA: it_list_ccusto TYPE TABLE OF ty_list_ccusto.

  DATA: wl_zpmt0001        TYPE zpmt0001,
        vl_low             TYPE tvarvc-low,
        t_cod_oper_cliente TYPE TABLE OF ty_cod_oper_cliente.

  BREAK rfreitas.

  CLEAR: wl_zpmt0001.
  MOVE-CORRESPONDING p_registro_manter TO wl_zpmt0001.

  DATA(lv_kostlv) = '%' && wl_zpmt0001-kostlg.

  SELECT * FROM itob
    INTO @DATA(wa_itob)
    UP TO 1 ROWS
    WHERE eqtyp EQ @wl_zpmt0001-eqtyp
      AND eqart EQ @wl_zpmt0001-eqart
      AND kostl LIKE @lv_kostlv.
  ENDSELECT.
  IF sy-subrc IS INITIAL AND sy-ucomm EQ 'CHANGE'.
    MESSAGE 'Registro já utilizado, edição não permitida!' TYPE 'E'.
    EXIT.
  ENDIF.


  SELECT *
        UP TO 1 ROWS
         FROM t370u
         INTO @DATA(wa_t370u)
         WHERE eqtyp  EQ @wl_zpmt0001-eqtyp.
  ENDSELECT.
  IF sy-subrc IS NOT INITIAL.
    MESSAGE 'Categoria Equipamento inválida!' TYPE 'E'.
    EXIT.
  ELSE.
    BREAK rfreitas.
    IF wa_t370u-eqtyp  NE '1'
    AND wa_t370u-eqtyp NE '2'
    AND wa_t370u-eqtyp NE '3'
    AND wa_t370u-eqtyp NE '4'
    AND wa_t370u-eqtyp NE 'A'.

      MESSAGE 'Categoria Equipamento não permitida!' TYPE 'E'.
      EXIT.

    ENDIF.
  ENDIF.

  SELECT *
        UP TO 1 ROWS
         FROM t370k_t
         INTO @DATA(wa_t370k_t)
         WHERE eqart  EQ @wl_zpmt0001-eqart.
  ENDSELECT.
  IF sy-subrc IS NOT INITIAL.
    MESSAGE 'Tipo Objeto inválido!' TYPE 'E'.
    EXIT.
  ENDIF.

  SELECT *
        UP TO 1 ROWS
         FROM zpmt0001
         INTO @DATA(wa_zpmt0001)
          WHERE eqtyp  EQ @wl_zpmt0001-eqtyp "FF #190351
            AND eqart  EQ @wl_zpmt0001-eqart
            AND kostlg EQ @wl_zpmt0001-kostlg.
  ENDSELECT.

  IF sy-ucomm EQ 'NOVO' OR sy-ucomm EQ 'CHANGE'.
    IF sy-subrc IS INITIAL.
*      MESSAGE 'Já criada na Categoria Equipamento!' TYPE 'E'.
      MESSAGE 'Já criado o centro de custo no tipo de objeto!' TYPE 'E'. "FF #190351
      EXIT.
    ENDIF.
  ENDIF.
*  ENDIF.

  IF sy-ucomm EQ 'CHANGE'.
    wl_zpmt0001-modificador = sy-uname.
    wl_zpmt0001-datam        = sy-datum.
    wl_zpmt0001-horam        = sy-uzeit.
  ENDIF.

*  data(lv_kostls) = '%' && wl_zpmt0001-kostlg.

*  select ltext
*        up to 1 rows
*         from cskt
*         into @data(lv_achou)
*         where spras eq 'P'
**           AND   ktopl EQ '0050'
*         and   kostl like @lv_kostls.
*  endselect.
*
*  if sy-ucomm eq 'NOVO' or sy-ucomm eq 'CHANGE'.
*    if sy-subrc is not initial.
*      message 'Centro de Custo não encontrado!' type 'E'.
*      exit.
*    endif.
*  endif.

  IF wl_zpmt0001-kostlg IS INITIAL.
    p_error = abap_true.
    MESSAGE i024(sd) WITH 'Preencha o centro de custo!'.
    EXIT.
  ELSE.
*    data(lv_kostls) = '%' && wl_zpmt0001-kostlg.
    SELECT * FROM csks INTO TABLE @DATA(it_csks) WHERE kosar EQ 'F'.
    IF sy-subrc EQ 0.
      SORT it_csks BY kostl.
      FREE: it_list_ccusto.
      it_list_ccusto = VALUE #( FOR l IN it_csks ( kostlg = l-kostl+6(4) ) ).
      SORT it_list_ccusto BY kostlg.
      DELETE ADJACENT DUPLICATES FROM it_list_ccusto COMPARING kostlg.
      READ TABLE it_list_ccusto INTO DATA(wa_list_ccusto) WITH KEY kostlg = wl_zpmt0001-kostlg.
      IF sy-subrc NE 0.
        IF wl_zpmt0001-kostlg NE '0196'. "Centro de custo Engenharia
          MESSAGE i024(sd) WITH 'O centro de custo não permitido!'.
          p_error = abap_true.
          EXIT.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

  MOVE-CORRESPONDING wl_zpmt0001 TO p_registro_manter.

  CLEAR: p_error.

ENDFORM.


FORM f_exit_zpmt0001_0003 CHANGING p_saida TYPE any.

*  DATA: wl_zpmt0001 TYPE zpmt0001.
*
*  CLEAR: wl_zpmt0001.
*
*  MOVE-CORRESPONDING p_saida TO wl_zpmt0001.
*
*  MOVE-CORRESPONDING wl_zpmt0001 TO p_saida.

  BREAK rfreitas.

  TYPES: BEGIN OF ty_cod_oper_cliente,
           low  TYPE tvarvc-low,
           high TYPE tvarvc-high,
         END   OF ty_cod_oper_cliente.

  DATA: wl_zpmt0001        TYPE zpmt0001,
        vl_low             TYPE tvarvc-low,
        t_cod_oper_cliente TYPE TABLE OF ty_cod_oper_cliente.

  CLEAR: wl_zpmt0001.
  MOVE-CORRESPONDING p_saida TO wl_zpmt0001.

  SELECT *
        UP TO 1 ROWS
         FROM t370u
         INTO @DATA(wa_t370u)
         WHERE eqtyp  EQ @wl_zpmt0001-eqtyp.
  ENDSELECT.
  IF sy-subrc IS NOT INITIAL.
    MESSAGE 'Categoria Equipamento inválida!' TYPE 'E'.
    EXIT.
  ELSE.
    BREAK rfreitas.
    IF wa_t370u-eqtyp NE '1'
    AND wa_t370u-eqtyp NE '2'
    AND wa_t370u-eqtyp NE '3'
    AND wa_t370u-eqtyp NE '4'
    AND wa_t370u-eqtyp NE 'A'.

      MESSAGE 'Categoria Equipamento não permitida!' TYPE 'E'.
      EXIT.

    ENDIF.

  ENDIF.

  SELECT *
        UP TO 1 ROWS
         FROM t370k_t
         INTO @DATA(wa_t370k_t)
         WHERE eqart  EQ @wl_zpmt0001-eqart.
  ENDSELECT.
  IF sy-subrc IS NOT INITIAL.
    MESSAGE 'Tipo Objeto inválido!' TYPE 'E'.
    EXIT.
  ENDIF.

  SELECT *
        UP TO 1 ROWS
         FROM zpmt0001
         INTO @DATA(wa_zpmt0001)
         WHERE eqtyp  EQ @wl_zpmt0001-eqtyp "FF #190351
         and   eqart EQ @wl_zpmt0001-eqart
         AND   kostlg EQ @wl_zpmt0001-kostlg.
  ENDSELECT.

  IF sy-ucomm EQ 'NOVO' OR sy-ucomm EQ 'CHANGE'.
    IF sy-subrc IS INITIAL.
*      MESSAGE 'Já criada na Categoria Equipamento!' TYPE 'E'.
      MESSAGE 'Já criado o centro de custo no tipo de objeto!' TYPE 'E'. ""FF #190351
      EXIT.
    ENDIF.
  ENDIF.
*  ENDIF.

  IF sy-ucomm EQ 'CHANGE'.
    wl_zpmt0001-modificador = sy-uname.
    wl_zpmt0001-datam        = sy-datum.
    wl_zpmt0001-horam        = sy-uzeit.
  ENDIF.

  DATA(lv_kostls) = '%' && wl_zpmt0001-kostlg.

  SELECT ltext
        UP TO 1 ROWS
         FROM cskt
         INTO @DATA(lv_achou)
         WHERE spras EQ 'P'
*           AND   ktopl EQ '0050'
         AND   kostl LIKE @lv_kostls.
  ENDSELECT.

  IF sy-ucomm EQ 'NOVO' OR sy-ucomm EQ 'CHANGE'.
    IF sy-subrc IS NOT INITIAL.
      MESSAGE 'Centro de Custo não encontrado!' TYPE 'E'.
      EXIT.
    ENDIF.
  ENDIF.

  MOVE-CORRESPONDING wl_zpmt0001 TO p_saida.

*  CLEAR: p_error.

ENDFORM.

FORM f_exit_zpmt0001_0004 CHANGING p_saida TYPE any.

  TYPES: BEGIN OF ty_cod_oper_cliente,
           low  TYPE tvarvc-low,
           high TYPE tvarvc-high,
         END   OF ty_cod_oper_cliente.

  TYPES: BEGIN OF ty_list_ccusto,
           kostl TYPE zkostlg,
           ktext TYPE cskt-ktext,
           ltext TYPE cskt-ltext,
         END OF ty_list_ccusto.

  DATA: it_list_ccusto TYPE TABLE OF ty_list_ccusto.

  DATA: wl_zpmt0001_out    TYPE zpmt0001_out.
*        vl_low             TYPE tvarvc-low,
*        t_cod_oper_cliente TYPE TABLE OF ty_cod_oper_cliente.

  CLEAR: wl_zpmt0001_out.
  MOVE-CORRESPONDING p_saida TO wl_zpmt0001_out.

  IF wl_zpmt0001_out-eqart IS NOT INITIAL.
    SELECT SINGLE eartx
           FROM t370k_t
           INTO wl_zpmt0001_out-desc_eqart
           WHERE spras EQ 'P'
           AND   eqart EQ wl_zpmt0001_out-eqart.
  ENDIF.

  IF wl_zpmt0001_out-kostlg IS NOT INITIAL.

*    data(lv_kostls) = '%' && wl_zpmt0001_out-kostlg.
*
*    select ltext
*          up to 1 rows
*           from cskt
*           into wl_zpmt0001_out-desc_kostl
*           where spras eq 'P'
**           AND   ktopl EQ '0050'
*           and   kostl like lv_kostls.
*    endselect.

    SELECT a~* FROM cskt AS a INNER JOIN csks AS b ON b~kostl EQ a~kostl
    INTO TABLE @DATA(it_csks) WHERE b~kosar EQ 'F' AND a~spras EQ @sy-langu.
    IF sy-subrc EQ 0.
      SORT it_csks BY kostl.
      FREE: it_list_ccusto.
      it_list_ccusto = VALUE #( FOR l IN it_csks ( kostl = l-kostl+6(4)
                                                   ktext = l-ktext
                                                   ltext = l-ltext ) ).
      SORT it_list_ccusto BY kostl.
      DELETE ADJACENT DUPLICATES FROM it_list_ccusto COMPARING kostl.
      READ TABLE it_list_ccusto INTO DATA(wa_list_ccusto) WITH KEY kostl = wl_zpmt0001_out-kostlg.
      IF sy-subrc EQ 0.
        wl_zpmt0001_out-desc_kostl = wa_list_ccusto-ktext.
      ELSEIF wl_zpmt0001_out-kostlg EQ '0196'.
        wl_zpmt0001_out-desc_kostl = 'Engenharia'.
      ENDIF.
    ENDIF.


  ENDIF.

  MOVE-CORRESPONDING wl_zpmt0001_out TO p_saida.

ENDFORM.

FORM f_exit_zpmt0001_0005 CHANGING p_registro_manter TYPE any.


  TYPES: BEGIN OF ty_cod_oper_cliente,
           low  TYPE tvarvc-low,
           high TYPE tvarvc-high,
         END   OF ty_cod_oper_cliente.

  DATA: wl_zpmt0001_out    TYPE zpmt0001_out,
        vl_low             TYPE tvarvc-low,
        t_cod_oper_cliente TYPE TABLE OF ty_cod_oper_cliente.

  CLEAR: wl_zpmt0001_out.
  MOVE-CORRESPONDING p_registro_manter TO wl_zpmt0001_out.

  IF wl_zpmt0001_out-eqart IS NOT INITIAL.
    SELECT SINGLE eartx
           FROM t370k_t
           INTO wl_zpmt0001_out-desc_eqart
           WHERE spras EQ 'P'
           AND   eqart EQ wl_zpmt0001_out-eqart.
  ENDIF.

  IF wl_zpmt0001_out-kostlg IS NOT INITIAL.

    DATA(lv_kostls) = '%' && wl_zpmt0001_out-kostlg.

    SELECT ltext
          UP TO 1 ROWS
           FROM cskt
           INTO wl_zpmt0001_out-desc_kostl
           WHERE spras EQ 'P'
*           AND   ktopl EQ '0050'
           AND   kostl LIKE lv_kostls.
    ENDSELECT.

*    IF sy-ucomm EQ 'NOVO' AND sy-ucomm EQ 'CHANGE'.
*      IF sy-subrc IS NOT INITIAL.
*        MESSAGE 'Centro de Custo não encontrado!' TYPE 'E'.
*        EXIT.
*      ENDIF.
*    ENDIF.

*zpmt0001
*EQTYP
*EQART
*KOSTLG

*    SELECT *
*          UP TO 1 ROWS
*           FROM zpmt0001
*           INTO @DATA(wa_zpmt0001)
*           WHERE eqtyp  EQ @wl_zpmt0001_out-eqtyp
*           AND   kostlg EQ @wl_zpmt0001_out-kostlg.
*    ENDSELECT.
*
*    IF sy-ucomm EQ 'NOVO' AND sy-ucomm EQ 'CHANGE'.
*      IF sy-subrc IS INITIAL.
*        MESSAGE 'Já criada na Categoria Equipamento!' TYPE 'E'.
*        EXIT.
*      ENDIF.
*    ENDIF.

  ENDIF.

  MOVE-CORRESPONDING wl_zpmt0001_out TO p_registro_manter.

ENDFORM.
FORM f_exit_zpmt0001_0008 CHANGING p_col_pos
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

  IF p_ref_tabname = 'ZPMT0001_OUT'. "= 'ZSDT0343 T2'.
*     p_field       = 'STEXT'.
    p_outputlen = 20.
  ENDIF.

  IF  p_ref_tabname = 'ZPMT0001_OUT' "= 'ZSDT0343 T2'.
  AND p_field       = 'DATAM'.
    p_scrtext_l = 'Data Modificação'.
    p_outputlen = 20.
  ENDIF.

  IF  p_ref_tabname = 'ZPMT0001_OUT' "= 'ZSDT0343 T2'.
  AND p_field       = 'HORAM'.
    p_scrtext_l = 'Hora Modificação'.
    p_outputlen = 20.
  ENDIF.

ENDFORM.
FORM  f_exit_zpmt0001_0009 TABLES it_excl_toolbar
                           USING p_db_tab.
  IF p_db_tab = 'ZPMT0001'.
    APPEND 'Modificar'    TO it_excl_toolbar.
  ENDIF.
ENDFORM.
FORM f_exit_zpmt0001_0014 USING p_saida TYPE any
                          CHANGING p_break TYPE any.

  DATA: wl_zpmt0001_out    TYPE zpmt0001_out.

  CLEAR: wl_zpmt0001_out.
  MOVE-CORRESPONDING p_saida TO wl_zpmt0001_out.

  BREAK rfreitas.

  DATA(lv_kostlv) = '%' && wl_zpmt0001_out-kostlg.

  SELECT * FROM itob
    INTO @DATA(wa_itob)
    UP TO 1 ROWS
    WHERE eqtyp EQ @wl_zpmt0001_out-eqtyp
      AND eqart EQ @wl_zpmt0001_out-eqart
      AND kostl LIKE @lv_kostlv.
  ENDSELECT.
  IF sy-subrc IS INITIAL.

    "FF #188919 - inicio

    DATA:
      lv_objnr  TYPE jest-objnr,
      lv_status TYPE jest-stat VALUE 'I0320'. "Inativo

    lv_objnr = wa_itob-objnr.

    CALL FUNCTION 'STATUS_CHECK'
      EXPORTING
        bypass_buffer     = abap_true
        objnr             = lv_objnr
        status            = lv_status
      EXCEPTIONS
        object_not_found  = 1
        status_not_active = 2
        OTHERS            = 3.

    IF sy-subrc <> 0. "Só permitir a exclusão se o equipamento vinculado ao centro de custo estiver inativo.
      "FF #188919 - fim

      MESSAGE 'Registro já utilizado, exclusão não permitida.' TYPE 'I' DISPLAY LIKE 'E'.
      p_break = abap_true.

    ENDIF.

  ENDIF.

  MOVE-CORRESPONDING wl_zpmt0001_out TO p_saida.

ENDFORM.



FORM f_exit_zpmt0001_0017 USING p_tipo.

  CALL FUNCTION 'SAPGUI_SET_FUNCTIONCODE'
    EXPORTING
      functioncode           = '=ENTER' "ENTER
    EXCEPTIONS
      function_not_supported = 1
      OTHERS                 = 2.

  IF p_tipo = '0001'.
    PERFORM f4_value USING '<FS_WA_REGISTRO_MANTER>-KOSTLG' p_tipo.

  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form f4_value
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_
*&      --> P_TIPO
*&---------------------------------------------------------------------*
FORM f4_value  USING p_campo TYPE help_info-dynprofld
                     p_tipo.

  TYPES: BEGIN OF ty_list_ccusto,
           kostlg TYPE zkostlg,
           ktext  TYPE cskt-ktext,
           ltext  TYPE cskt-ltext,
         END OF ty_list_ccusto.

  DATA: it_list_ccusto TYPE TABLE OF ty_list_ccusto.


  DATA: zde_fieldname    TYPE dfies-fieldname,
        zde_window_title TYPE c,
        value_tab        TYPE value_tab.

  DATA: t_return  TYPE STANDARD TABLE OF ddshretval,
        t_mapping TYPE STANDARD TABLE OF dselc.


  CASE p_tipo.
    WHEN '0001'. "C.custo.

      CLEAR t_return.

      SELECT a~kostl, a~ktext, a~ltext
       FROM cskt AS a
       INNER JOIN csks AS b
       ON b~kostl EQ a~kostl
       AND b~kosar EQ 'F'
       INTO TABLE @DATA(lt_cskt)
       WHERE a~spras EQ @sy-langu.
      IF sy-subrc = 0.

        SORT lt_cskt BY kostl.
        FREE: it_list_ccusto.
        it_list_ccusto = VALUE #( FOR l IN lt_cskt ( kostlg = l-kostl+6(4)
                                                     ktext = l-ktext
                                                     ltext = l-ltext ) ). "0150070325.
        IF it_list_ccusto IS NOT INITIAL..
          SORT it_list_ccusto BY kostlg.
          DELETE ADJACENT DUPLICATES FROM it_list_ccusto COMPARING kostlg.

          CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
            EXPORTING
              retfield        = 'KOSTLG'
              dynpprog        = sy-cprog
              dynpnr          = sy-dynnr
              dynprofield     = p_campo
              window_title    = 'Tipo do objeto técnico'
              value_org       = 'S'
            TABLES
              value_tab       = it_list_ccusto
              return_tab      = t_return
              dynpfld_mapping = t_mapping
            EXCEPTIONS
              parameter_error = 1
              no_values_found = 2
              OTHERS          = 3.
          IF sy-subrc <> 0.
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          ENDIF.
        ENDIF.
      ENDIF.

  ENDCASE.

  CALL FUNCTION 'SAPGUI_SET_FUNCTIONCODE'
    EXPORTING
      functioncode           = '=ENTER' "ENTER
    EXCEPTIONS
      function_not_supported = 1
      OTHERS                 = 2.

ENDFORM.

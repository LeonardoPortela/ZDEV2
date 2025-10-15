CLASS zcl_pp_util DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS get_lote_codigo
      IMPORTING
        !is_caufvd TYPE caufvd
      RETURNING
        VALUE(rv_lote) TYPE string
      RAISING
        cx_static_check.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_pp_util IMPLEMENTATION.

  METHOD get_lote_codigo.

    DATA(lv_ano) = is_caufvd-gstrp+0(4).

    " Busca característica no STVARV
    SELECT SINGLE low
      INTO @DATA(lv_atnam)
      FROM tvarvc
      WHERE name = 'Z_CARACTERISTICA_LOTE'.

    IF sy-subrc <> 0 OR lv_atnam IS INITIAL.
      MESSAGE |Característica Z_CARACTERISTICA_LOTE não encontrada. Acesse a STVARV.| TYPE 'E'.
    ENDIF.

    " Busca código interno da característica
    SELECT SINGLE atinn
      INTO @DATA(lv_atinn)
      FROM cabn
      WHERE atnam = @lv_atnam.

    IF sy-subrc <> 0.
      MESSAGE |Característica { lv_atnam } não encontrada.| TYPE 'E'.
    ENDIF.

    " Busca valor da característica para o material
    SELECT SINGLE atwrt
      INTO @DATA(lv_atwrt)
      FROM ausp
      WHERE objek = @is_caufvd-plnbez
        AND atinn = @lv_atinn
        AND klart = '001'.

    IF sy-subrc <> 0.
      MESSAGE |Material { is_caufvd-plnbez } deve possuir a característica { lv_atnam }.| TYPE 'E'.
    ENDIF.

    DATA(lv_cod_caract) = lv_atwrt+0(2).

    " Período do ano atual
    DATA(lv_data_inicio) = CONV dats( |{ sy-datum+0(4) }0101| ).
    DATA(lv_data_fim)    = CONV dats( |{ sy-datum+0(4) }1231| ).

    " Busca lotes existentes no ano para o material/centro
    SELECT charg
      INTO TABLE @DATA(lt_mcha)
      FROM mcha
      WHERE matnr = @is_caufvd-plnbez
        AND werks = @is_caufvd-werks
        AND ersda BETWEEN @lv_data_inicio AND @lv_data_fim.

    DATA lv_sufixo(4) TYPE n VALUE '0001'.

    IF lt_mcha IS NOT INITIAL.
      DATA: lt_codigos      TYPE STANDARD TABLE OF i,
            lv_sufix_aux(4) TYPE n.

      LOOP AT lt_mcha INTO DATA(ls_mcha).
        IF ls_mcha-charg CP |{ lv_ano }{ lv_cod_caract }*|.
          lv_sufix_aux = ls_mcha-charg+6(4).
          APPEND lv_sufix_aux TO lt_codigos.
        ENDIF.
      ENDLOOP.

      SORT lt_codigos DESCENDING.
      READ TABLE lt_codigos INDEX 1 INTO DATA(lv_maior).
      IF sy-subrc = 0.
        lv_sufixo = lv_maior + 1.
        UNPACK lv_sufixo TO lv_sufixo.
      ENDIF.
    ENDIF.

    rv_lote = |{ lv_ano }{ lv_cod_caract }{ lv_sufixo }|.

  ENDMETHOD.

ENDCLASS.


REPORT zfabricio.


BREAK-POINT.

data tp_veiculo TYPE itob-eqart VALUE 526.


    SELECT *
      FROM ztparam
      INTO @DATA(ls_dummy)
      UP TO 1 ROWS
      WHERE param = 'TP_IMPLEM'
        AND zval  = @tp_veiculo.
    ENDSELECT.

    BREAK-POINT.

 if sy-subrc = 0.


 endif.


*
*DATA: lv_ano        TYPE string VALUE '2025',
*      lv_cod_caract TYPE string VALUE 'AB',
*      lv_sufixo(4)  TYPE n VALUE '0001',
*      lv_lote_final TYPE string.
*
*" Simulação da tabela MCHA (campos reduzidos)
*TYPES: BEGIN OF ty_mcha,
*         charg TYPE string,
*       END OF ty_mcha.
*
*DATA: lt_mcha TYPE STANDARD TABLE OF ty_mcha.
*
*"--- Popula a lt_mcha com lotes existentes
*APPEND VALUE #( charg = '2025AB0001' ) TO lt_mcha.
*
*
*"--- Sua lógica de geração de próximo sufixo ---
*IF lt_mcha IS NOT INITIAL.
*  DATA: lt_codigos      TYPE STANDARD TABLE OF i,
*        lv_sufix_aux(4) TYPE n.
*
*  LOOP AT lt_mcha INTO DATA(ls_mcha).
*    IF ls_mcha-charg CP |{ lv_ano }{ lv_cod_caract }*|.
*      lv_sufix_aux = ls_mcha-charg+6(4).
*      APPEND lv_sufix_aux TO lt_codigos.
*    ENDIF.
*  ENDLOOP.
*
*  SORT lt_codigos DESCENDING.
*  READ TABLE lt_codigos INDEX 1 INTO DATA(lv_maior).
*  IF sy-subrc = 0.
*    lv_sufixo = lv_maior + 1 .
*    UNPACK lv_sufixo TO lv_sufixo.
*  ENDIF.
*
*ENDIF.
*
*lv_lote_final = |{ lv_ano }{ lv_cod_caract }{ lv_sufixo }|.
*
*WRITE: / 'Próximo lote gerado:', lv_lote_final.

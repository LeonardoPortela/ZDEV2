class ZCL_DEPARA_CENTRO_FIXO_AFIXAR definition
  public
  final
  create public .

public section.

  interfaces ZIF_DEPARA_CENTRO_FIXO_AFIXAR .
protected section.
private section.
ENDCLASS.



CLASS ZCL_DEPARA_CENTRO_FIXO_AFIXAR IMPLEMENTATION.


  METHOD zif_depara_centro_fixo_afixar~get_dados_depara.
    DATA: lra_matnr         TYPE RANGE OF matnr,
          lra_centro_afixar TYPE RANGE OF werks_d,
          lra_centro_fixo   TYPE RANGE OF werks_d,
          lra_deposito      TYPE RANGE OF lgort_d,
          lra_tipo_produto  TYPE RANGE OF zde_tp_produto,
          lra_eudr          TYPE RANGE OF zeudr.

    CLEAR: e_table_depara[], e_single_depara.

    IF i_material IS NOT INITIAL.
      APPEND VALUE #( sign = 'I' option = 'EQ' low = i_material ) TO lra_matnr.
    ELSEIF it_matnr IS NOT INITIAL.
      LOOP AT it_matnr INTO DATA(wa_matnr).
        APPEND VALUE #( sign = 'I' option = 'EQ' low = wa_matnr ) TO lra_matnr.
      ENDLOOP.
    ENDIF.

    IF i_centro_fixo IS NOT INITIAL.
      APPEND VALUE #( sign = 'I' option = 'EQ' low = i_centro_fixo ) TO lra_centro_fixo.
    ELSEIF it_centro_fixo IS NOT INITIAL.
      LOOP AT it_matnr INTO DATA(wa_centro_fixo).
        APPEND VALUE #( sign = 'I' option = 'EQ' low = wa_centro_fixo ) TO lra_centro_fixo.
      ENDLOOP.
    ENDIF.

    IF i_centro_afixar IS NOT INITIAL.
      APPEND VALUE #( sign = 'I' option = 'EQ' low = i_centro_afixar ) TO lra_centro_afixar.
    ELSEIF it_centro_afixar IS NOT INITIAL.
      LOOP AT it_centro_afixar INTO DATA(wa_centro_afixar).
        APPEND VALUE #( sign = 'I' option = 'EQ' low = wa_centro_afixar ) TO lra_centro_afixar.
      ENDLOOP.
    ENDIF.

    IF i_deposito IS NOT INITIAL.
      APPEND VALUE #( sign = 'I' option = 'EQ' low = i_deposito ) TO lra_deposito.
    ELSEIF it_deposito IS NOT INITIAL.
      LOOP AT it_deposito INTO DATA(wa_deposito).
        APPEND VALUE #( sign = 'I' option = 'EQ' low = wa_deposito ) TO lra_deposito.
      ENDLOOP.
    ENDIF.

    IF i_tipo_produto IS NOT INITIAL.
      APPEND VALUE #( sign = 'I' option = 'EQ' low = i_tipo_produto ) TO lra_tipo_produto.
    ENDIF.

    IF i_eudr IS NOT INITIAL.
      APPEND VALUE #( sign = 'I' option = 'EQ' low = i_eudr ) TO lra_eudr.
    ENDIF.


    SELECT SINGLE *
    FROM zmmt0017
    INTO e_single_depara
    WHERE matnr          IN lra_matnr
      AND centro_a_fixar IN lra_centro_afixar
      AND centro_fixo    IN lra_centro_fixo
      AND lgort          IN lra_deposito
      AND tp_produto     IN lra_tipo_produto
      AND eudr           IN lra_eudr.

    SELECT  *
    FROM zmmt0017
    INTO TABLE e_table_depara
    WHERE matnr          IN lra_matnr
      AND centro_a_fixar IN lra_centro_afixar
      AND centro_fixo    IN lra_centro_fixo
      AND lgort          IN lra_deposito
      AND tp_produto     IN lra_tipo_produto
      AND eudr           IN lra_eudr.


  ENDMETHOD.


  method ZIF_DEPARA_CENTRO_FIXO_AFIXAR~GET_INSTANCE.

*    IF ZIF_DEPARA_CENTRO_FIXO_AFIXAR~AT_centro IS NOT BOUND.
*      CREATE OBJECT ZIF_DEPARA_CENTRO_FIXO_AFIXAR~AT_centro TYPE ZCL_DEPARA_CENTRO_FIXO_AFIXAR.
*    ENDIF.
*
*    R_INSTANCE = ZIF_DEPARA_CENTRO_FIXO_AFIXAR~AT_centro.
  endmethod.
ENDCLASS.

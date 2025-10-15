class ZCL_DEPARA_CENTRO_FIXO_VIRTUAL definition
  public
  final
  create public .

public section.

  class-methods GET_DADOS_DEPARA
    importing
      !I_WERKS type WERKS_D optional
      !IT_WERKS type ZMMTT_WERKS optional
      !I_WERKS_V type WERKS_D optional
      !I_LIFNR type LIFNR optional
      !IT_LIFNR type T_RANGE_LIFNR optional
      !I_OPERACAO type ZDE_OPER_AQUAV optional
      !IT_OPERACAO type ZT_OPERACAO optional
      !I_LGORT type LGORT_D optional
      !IT_LGORT type ZMMTT_LGORT optional
      !I_EUDR type ZEUDR optional
    exporting
      !E_TABLE_DEPARA type ZSDT_DEPARA_DEPO_TP
      !E_SINGLE_DEPARA type ZSDT_DEPARA_DEPO .
protected section.
private section.
ENDCLASS.



CLASS ZCL_DEPARA_CENTRO_FIXO_VIRTUAL IMPLEMENTATION.


  METHOD get_dados_depara.
    DATA: lra_centro   TYPE RANGE OF werks_d,
          lra_centro_v TYPE RANGE OF werks_d,
          lra_lifnr    TYPE RANGE OF lifnr,
          lra_operacao TYPE RANGE OF zde_oper_aquav,
          lra_deposito TYPE RANGE OF lgort_d,
          lra_eudr     TYPE RANGE OF zeudr.

    CLEAR: e_single_depara, e_table_depara[].

    IF i_werks IS NOT INITIAL.
      APPEND VALUE #( sign = 'I' option = 'EQ' low = i_werks ) TO lra_centro.
    ELSEIF it_werks IS NOT INITIAL.
      LOOP AT it_werks INTO DATA(wa_centro).
        APPEND VALUE #( sign = 'I' option = 'EQ' low = wa_centro ) TO lra_centro.
      ENDLOOP.
    ENDIF.

    IF i_werks_v IS NOT INITIAL.
      APPEND VALUE #( sign = 'I' option = 'EQ' low = i_werks_v ) TO lra_centro_v.
    ENDIF.

    IF i_lifnr IS NOT INITIAL.
      APPEND VALUE #( sign = 'I' option = 'EQ' low = i_lifnr ) TO lra_lifnr.
    ELSEIF it_lifnr IS NOT INITIAL.
      LOOP AT it_lifnr INTO DATA(wa_lifnr).
        APPEND VALUE #( sign = 'I' option = 'EQ' low = wa_lifnr ) TO lra_lifnr.
      ENDLOOP.
    ENDIF.

    IF i_operacao IS NOT INITIAL.
      APPEND VALUE #( sign = 'I' option = 'EQ' low = i_operacao ) TO lra_operacao.
    ELSEIF it_operacao IS NOT INITIAL.
      LOOP AT it_operacao INTO DATA(wa_operacao).
        APPEND VALUE #( sign = 'I' option = 'EQ' low = wa_operacao ) TO lra_operacao.
      ENDLOOP.
    ENDIF.


    IF i_lgort IS NOT INITIAL.
      APPEND VALUE #( sign = 'I' option = 'EQ' low = i_lgort ) TO lra_deposito.
    ELSEIF it_lgort IS NOT INITIAL.
      LOOP AT it_lgort INTO DATA(wa_deposito).
        APPEND VALUE #( sign = 'I' option = 'EQ' low = wa_deposito ) TO lra_deposito.
      ENDLOOP.
    ENDIF.

    IF i_eudr EQ 'S'.
      APPEND VALUE #( sign = 'I' option = 'EQ' low = 'S' ) TO lra_eudr.
    ELSE.
      APPEND VALUE #( sign = 'I' option = 'NE' low = 'S' ) TO lra_eudr.
    ENDIF.

    SELECT SINGLE *
      FROM zsdt_depara_depo INTO e_single_depara
     WHERE werks    IN lra_centro
       AND werks_v  IN lra_centro_v
       AND lifnr    IN lra_lifnr
       AND operacao IN lra_operacao
       AND eudr     IN lra_eudr
       AND lgort    IN lra_deposito.

    SELECT *
      FROM zsdt_depara_depo INTO TABLE e_table_depara
     WHERE werks    IN lra_centro
       AND werks_v  IN lra_centro_v
       AND lifnr    IN lra_lifnr
       AND operacao IN lra_operacao
       AND eudr     IN lra_eudr
       AND lgort    IN lra_deposito.

  ENDMETHOD.
ENDCLASS.

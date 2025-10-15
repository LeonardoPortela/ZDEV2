class ZCL_SD_HEADER_TO definition
  public
  final
  create public .

public section.

  types TY_T_TEXTO type STRING_TABLE .

  constants C_TRACO type CHAR5 value ' - ' ##NO_TEXT.
  constants C_VIRGULA type CHAR5 value ', ' ##NO_TEXT.
  constants C_SPRAS_EN type T005T-SPRAS value 'E' ##NO_TEXT.

  methods CONSTRUCTOR
    importing
      !I_W_ADRC type ADRC
      !I_V_SEPARADOR_DOM_CIDADE_PAIS type CHAR5 optional
      !I_V_SEPARADOR_RUA_NUM_BAIRO type CHAR5 optional
      !I_V_PAIS_EXTENSO type BOOLEAN optional .
  methods GET_W_HEADER_TO
    returning
      value(R_W_HEADER_TO) type ZES_SD_HEADER_TO .
protected section.
private section.

  data W_ADRC type ADRC .
  data V_PAIS type STRING .
  data W_HEADER_TO type ZES_SD_HEADER_TO .
  data V_SEPARADOR_DOM_CIDADE_PAIS type CHAR5 .
  data V_SEPARADOR_RUA_NUM_BAIRO type CHAR5 .

  methods PREENCHER_RUA_NUM_BAIRO .
  methods PREENCHER_DOM_CIDADE_PAIS .
  methods PREENCHER_V_PAIS_EXTENSO
    returning
      value(R_V_PAIS) type STRING .
  methods CONCATENAR_TEXTO
    importing
      !I_T_TEXTO type TY_T_TEXTO
      !I_V_SEPARADOR type CHAR5
    returning
      value(R_V_RESULTADO) type STRING .
ENDCLASS.



CLASS ZCL_SD_HEADER_TO IMPLEMENTATION.


  METHOD concatenar_texto.

    LOOP AT i_t_texto INTO DATA(texto).

      IF texto IS NOT INITIAL.

        IF r_v_resultado IS INITIAL.

          r_v_resultado = texto.

        ELSE.

          CONCATENATE r_v_resultado texto
                  INTO r_v_resultado
                  SEPARATED BY i_v_separador.

        ENDIF.

      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD constructor.

    me->w_adrc                      = i_w_adrc.
    me->v_separador_rua_num_bairo   = i_v_separador_rua_num_bairo.
    me->v_separador_dom_cidade_pais = i_v_separador_dom_cidade_pais.

    IF i_v_pais_extenso IS NOT INITIAL.
      me->v_pais = me->preencher_v_pais_extenso( ).
    ELSE.
      me->v_pais = me->w_adrc-country.
    ENDIF.

    IF me->v_separador_rua_num_bairo IS INITIAL.
      me->v_separador_rua_num_bairo = me->c_virgula.
    ENDIF.

    IF me->v_separador_dom_cidade_pais IS INITIAL.
      me->v_separador_dom_cidade_pais = me->c_traco.
    ENDIF.

    me->preencher_rua_num_bairo( ).
    me->preencher_dom_cidade_pais( ).

  ENDMETHOD.


  METHOD get_w_header_to.

    r_w_header_to = me->w_header_to.

  ENDMETHOD.


  METHOD preencher_dom_cidade_pais.

    DATA:
          lt_texto TYPE ty_t_texto.

    APPEND  me->w_adrc-post_code1 TO lt_texto.
    APPEND  me->w_adrc-city1      TO lt_texto.
    APPEND  me->v_pais            TO lt_texto.

    me->w_header_to-dom_cidade_pais = me->concatenar_texto(
                                          i_t_texto     = lt_texto
                                          i_v_separador = me->v_separador_dom_cidade_pais
                                          ).

  ENDMETHOD.


  METHOD preencher_rua_num_bairo.

    DATA:
          lt_texto TYPE ty_t_texto.

    APPEND  me->w_adrc-street     TO lt_texto.
    APPEND  me->w_adrc-house_num1 TO lt_texto.
    APPEND  me->w_adrc-city2      TO lt_texto.

    me->w_header_to-rua_num_bairo = me->concatenar_texto(
                                          i_t_texto     = lt_texto
                                          i_v_separador = me->v_separador_rua_num_bairo
                                          ).

  ENDMETHOD.


  METHOD preencher_v_pais_extenso.

    SELECT SINGLE landx
      INTO @DATA(lv_landx)
      FROM t005t
      WHERE land1 = @me->w_adrc-country
        AND spras = @me->c_spras_en.

    IF sy-subrc IS INITIAL.

      r_v_pais = lv_landx.
      TRANSLATE r_v_pais TO UPPER CASE.

    ENDIF.

  ENDMETHOD.
ENDCLASS.

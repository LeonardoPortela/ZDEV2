"Name: \PR:SAPLV61A\FO:USEREXIT_XKOMV_FUELLEN\SE:BEGIN\EI
ENHANCEMENT 0 Z_MODIFY_CONDITION_PRICE.

  DATA: v_sy_subrc TYPE i.

  v_sy_subrc = sy-subrc.

*-----------------------------------------------------------------------------------------*
*---------------------------------| ATENÇÃO  |--------------------------------------------*
*
* Variavel ambiente SY-SUBRC, tem que sair dessa enhacement com o mesmo valor que entrou,
* pois é feito uma tratativa fora da exit com o valor retornado
*-----------------------------------------------------------------------------------------*

*-#133089-21.02.2024-JT-inicio
  DATA: t_callstack  TYPE abap_callstack,
        lc_fat_autom TYPE char01.

  CALL FUNCTION 'SYSTEM_CALLSTACK'
    EXPORTING
      max_level = 0
    IMPORTING
      callstack = t_callstack.

*------------------
* faturamento automatico
*------------------
  READ TABLE t_callstack INTO DATA(w_callstack) WITH KEY mainprogram = 'ZLESR0180_JOB'.
  IF sy-subrc = 0.
    lc_fat_autom = abap_true.
  ENDIF.
*-#133089-21.02.2024-JT-fim

  DO 1 TIMES.

    CHECK ( sy-tcode EQ 'VI01'      ) OR
          ( sy-tcode EQ 'VI02'      ) OR
          ( sy-tcode EQ 'VT01N'     ) OR
          ( sy-tcode EQ 'VT01'      ) OR
          ( sy-tcode EQ 'VT02N'     ) OR
          ( sy-tcode EQ 'VT02'      ) OR
          ( sy-tcode EQ 'ZLES0106'  ) OR
          ( sy-tcode EQ 'ZLES0113'  ) OR
          ( sy-tcode EQ 'ZMM0079'   ) OR
          ( sy-tcode EQ 'ZMM0127'   ) OR
          ( sy-tcode EQ 'ZLES0136'  ) OR
          ( sy-tcode EQ 'ZLES0115'  ) OR
          ( lc_fat_autom = abap_true ).

    FIELD-SYMBOLS: <field>  TYPE ANY TABLE.

    "Internal Table e Work Area
    DATA: ti_vfsi        TYPE TABLE OF vfsivb,
          ti_vttp        TYPE TABLE OF vttp,
          ti_itens_custo TYPE v54a0_scd_item_tab.

    "Variaveis
    DATA: vl_field(30) TYPE c.

    CHECK xkomv-kinak IS INITIAL. "Condição está ativa

    CLEAR: ti_vttp[], ti_itens_custo[].

*    IF TI_VTTP[] IS INITIAL.
**     Recuperar Itens VT pelos docs de fornecimento
*
**     Determinação do documento de transporte
*      VL_FIELD = '(SAPLV54B)G_VFSI[]'.
*      ASSIGN (VL_FIELD) TO <FIELD>.
*
*      CHECK <FIELD> IS ASSIGNED.
*
*      CLEAR: VL_FIELD, TI_VFSI, TI_VFSI[], TI_VTTP[].
*
**     Controle de Fornecimento
*      TI_VFSI[] = <FIELD>[].
*
*      CHECK TI_VFSI[] IS NOT INITIAL.
*
**     Condições: dados independentes da dimensão
*      SELECT *
*        FROM VTTP INTO TABLE TI_VTTP
*         FOR ALL ENTRIES IN TI_VFSI
*       WHERE VBELN = TI_VFSI-VBELN.
*
*      DELETE ADJACENT DUPLICATES FROM TI_VTTP.
*
*    ENDIF.

    "Get VT Atual...
    vl_field = '(SAPMV54A)G_SCD_WA-X-ITEM[]'.

    ASSIGN (vl_field) TO <field>.

    CHECK <field> IS ASSIGNED.

    ti_itens_custo[] = <field>[].

    LOOP AT ti_itens_custo INTO DATA(wl_item_custo) WHERE vfkp-rebel IS NOT INITIAL.
      SELECT *
        FROM vttp INTO TABLE ti_vttp
       WHERE tknum EQ wl_item_custo-vfkp-rebel.

      IF ti_vttp[] IS NOT INITIAL.
        EXIT.
      ENDIF.
    ENDLOOP.

    CHECK ti_vttp[] IS NOT INITIAL.

    CALL FUNCTION 'Z_MODIFY_CONDITION_PRICE'
      TABLES
        t_vttp  = ti_vttp
      CHANGING
        c_xkomv = xkomv.

  ENDDO.

  sy-subrc = v_sy_subrc.

ENDENHANCEMENT.

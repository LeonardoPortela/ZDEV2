class ZCL_CONTROLE_CONTA_RAZAO definition
  public
  final
  create public .

public section.

  class-data AT_CONTROLE_CONTA_RAZAO type ref to ZCL_CONTROLE_CONTA_RAZAO .

  class-methods GET_INSTANCE
    returning
      value(R_INSTANCE) type ref to ZCL_CONTROLE_CONTA_RAZAO .
  methods GET_CONTA_RAZAO
    importing
      !I_SHTYP type SHTYP
      !I_TCODE type TCODE
      !I_FATURA type ZFATURA
      !I_TP_EMISSOR type ZTP_EMISSOR
      !I_OPERFRETE type ZOPERFRETE optional
      !I_OPERFRETE_RANGE type ZDE_ZOPERFRETE_RANGES_T optional
      !I_TP_VEICULO type ZDE_TP_PROP_VEICULO_RANGES_T
      !I_DT_REFERENCIA type BUDAT
    exporting
      !E_ZLEST0021 type ZLEST0021
      !E_IT_ZLEST0021 type ZDE_ZLEST0021_T
    returning
      value(R_INSTANCE) type ref to ZCL_CONTROLE_CONTA_RAZAO
    raising
      ZCX_CONTROLE_CONTA_RAZAO .
protected section.
private section.
ENDCLASS.



CLASS ZCL_CONTROLE_CONTA_RAZAO IMPLEMENTATION.


  METHOD get_conta_razao.

    DATA: lc_zlest0021 TYPE zlest0021.

    CLEAR: e_zlest0021, e_it_zlest0021[], e_it_zlest0021.

    DATA(lc_operfrete_range) = i_operfrete_range[].

    IF i_operfrete IS NOT INITIAL.
      APPEND VALUE #( sign = 'I' option = 'EQ' low = i_operfrete high = i_operfrete ) TO lc_operfrete_range.
    ENDIF.

    DATA: lc_dt_vencimento TYPE zlest0021-dt_vencimento.

    "Checa Data Inicial
    SELECT SINGLE * INTO @DATA(wa_zlest0021)
      FROM zlest0021
     WHERE dt_vencimento EQ @lc_dt_vencimento.

    IF sy-subrc IS INITIAL.
      UPDATE zlest0021
         SET dt_vencimento = '99991231'
       WHERE dt_vencimento EQ lc_dt_vencimento.
    ENDIF.

    SELECT * INTO TABLE @DATA(it_zlest0021)
      FROM zlest0021
     WHERE shtyp         EQ @i_shtyp
       AND tcode         EQ @i_tcode
       AND fatura        EQ @i_fatura
       AND tp_emissor    EQ @i_tp_emissor
       AND operfrete     IN @lc_operfrete_range
       AND tp_veiculo    IN @i_tp_veiculo
       AND dt_vencimento GE @i_dt_referencia.

    IF sy-subrc IS NOT INITIAL.
      "Conta Razão não cadastrada para ChvId &MSGV1& e tipo transporte &MSGV2&
      RAISE EXCEPTION TYPE zcx_controle_conta_razao
        EXPORTING
          textid = VALUE #( msgid = zcx_controle_conta_razao=>zcx_nao_encontrado-msgid
                            msgno = zcx_controle_conta_razao=>zcx_nao_encontrado-msgno
                            attr1 = CONV #( i_operfrete )
                            attr2 = CONV #( i_shtyp ) )
          msgid  = zcx_controle_conta_razao=>zcx_nao_encontrado-msgid
          msgno  = zcx_controle_conta_razao=>zcx_nao_encontrado-msgno
          msgty  = 'E'
          msgv1  = CONV #( i_operfrete )
          msgv2  = CONV #( i_shtyp ).
    ENDIF.

*    IF I_OPERFRETE_RANGE IS NOT INITIAL.
    DATA(it_zlest0021_aux) = it_zlest0021[].
    SORT it_zlest0021 BY operfrete.
*---> 04/07/2023 - Migração S4 - WS
    SORT it_zlest0021_aux BY operfrete.
*<--- 04/07/2023 - Migração S4 - WS
    DELETE ADJACENT DUPLICATES FROM it_zlest0021_aux COMPARING operfrete.
    LOOP AT it_zlest0021_aux INTO DATA(wa_zlest0021_aux).
      CLEAR: lc_zlest0021.
      LOOP AT it_zlest0021 INTO wa_zlest0021 WHERE operfrete EQ wa_zlest0021_aux-operfrete.
        IF lc_zlest0021 IS INITIAL.
          lc_zlest0021 = wa_zlest0021.
        ELSEIF wa_zlest0021-dt_vencimento LT lc_zlest0021-dt_vencimento.
          lc_zlest0021 = wa_zlest0021.
        ENDIF.
      ENDLOOP.
      APPEND lc_zlest0021 TO e_it_zlest0021.
    ENDLOOP.

    IF i_operfrete IS NOT INITIAL.
      READ TABLE e_it_zlest0021 INDEX 1 INTO e_zlest0021.
    ENDIF.

*    ELSE.
*      SORT IT_ZLEST0021 BY DT_VENCIMENTO ASCENDING.
*      READ TABLE IT_ZLEST0021 INDEX 1 INTO E_ZLEST0021.
*    ENDIF.

  ENDMETHOD.


  METHOD GET_INSTANCE.

    IF ZCL_CONTROLE_CONTA_RAZAO=>AT_CONTROLE_CONTA_RAZAO IS NOT BOUND.
      CREATE OBJECT ZCL_CONTROLE_CONTA_RAZAO=>AT_CONTROLE_CONTA_RAZAO.
    ENDIF.

    R_INSTANCE = ZCL_CONTROLE_CONTA_RAZAO=>AT_CONTROLE_CONTA_RAZAO.

  ENDMETHOD.
ENDCLASS.

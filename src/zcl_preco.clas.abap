class ZCL_PRECO definition
  public
  final
  create public .

public section.

  interfaces ZIF_PRECO .
protected section.
private section.
ENDCLASS.



CLASS ZCL_PRECO IMPLEMENTATION.


  METHOD zif_preco~get_preco_estoque.

    " I_VPRSV - Código de controle de preço
    " S - Preço standard
    " V -	Preço médio móvel/preço interno periódico

    DATA: v_data_base TYPE erdat.

    v_data_base =  sy-datum.

    CLEAR: r_preco.

    IF ( i_matnr IS INITIAL ) OR ( i_bwkey IS INITIAL ) OR
       ( i_vprsv IS INITIAL ) OR ( i_waers IS INITIAL ) OR
       ( i_vprsv IS NOT INITIAL AND i_vprsv NE 'V' AND i_vprsv NE 'S' ).

      RAISE EXCEPTION TYPE zcx_preco
        EXPORTING
          textid = VALUE #( msgid = zcx_preco=>zcx_erro_preco_estoque-msgid
                            msgno = zcx_preco=>zcx_erro_preco_estoque-msgno
                            attr1 = CONV #( 'Parâmetros não informados' )
                           )
          msgty  = 'E'
          msgno  = zcx_preco=>zcx_erro_preco_estoque-msgno
          msgid  = zcx_preco=>zcx_erro_preco_estoque-msgid
          msgv1  = CONV #( 'Parâmetros não informados' ).
    ENDIF.

    SELECT SINGLE kalnr
      FROM ckmlhd INTO @DATA(_kalnr)
     WHERE matnr EQ @i_matnr
       AND bwkey EQ @i_bwkey.

    IF ( sy-subrc NE 0 ) OR ( _kalnr IS INITIAL ).
      RAISE EXCEPTION TYPE zcx_preco
        EXPORTING
          textid = VALUE #( msgid = zcx_preco=>zcx_erro_preco_estoque-msgid
                            msgno = zcx_preco=>zcx_erro_preco_estoque-msgno
                            attr1 = CONV #( 'Ledger de material'                    )
                            attr2 = CONV #( 'registro de cabeçalho não encontrado!' )
                            attr3 = CONV #( |Material : { i_matnr } |               )
                            attr4 = CONV #( |Área Avaliação: { i_bwkey } |          )
                           )
          msgty  = 'E'
          msgno  = zcx_preco=>zcx_erro_preco_estoque-msgno
          msgid  = zcx_preco=>zcx_erro_preco_estoque-msgid
          msgv1  = CONV #( 'Ledger de material' )
          msgv2  = CONV #( 'registro de cabeçalho não encontrado!' )
          msgv3  = CONV #( |Material : { i_matnr } | )
          msgv4  = CONV #( |Área Avaliação: { i_bwkey } | ).
    ENDIF.

    DATA(_encontrou_preco) = abap_false.


    DATA(_ano) = v_data_base(4).
    DATA(_ano_limite_busca) = _ano - 10.
    DATA(_mes) = v_data_base+4(2).

    DATA(_meses_busca) = 0.
    DATA(_meses_limite_busca) = 12.

    IF i_last EQ abap_true.
      _meses_limite_busca = 120.
    ENDIF.

    WHILE ( _encontrou_preco  EQ abap_false          ) AND
          ( _ano              GE _ano_limite_busca   ) AND
          ( _meses_busca      LE _meses_limite_busca ).

      ADD 1 TO _meses_busca.

      IF _mes = 001.
        _ano = _ano - 1.
        _mes = 012.
      ELSE.
        _mes = _mes - 1.
      ENDIF.

* ---> S4 Migration - 06/07/2023 - DG
*      SELECT SINGLE *
*        FROM CKMLCR INTO @DATA(WL_CKMLCR)
*       WHERE KALNR EQ @_KALNR
*         AND BDATJ EQ @_ANO
*         AND POPER EQ @_MES
*         AND WAERS EQ @I_WAERS.
*
*      IF SY-SUBRC EQ 0.

      DATA: wa_kalnr  TYPE ckmv0_matobj_str,
            lt_kalnr  TYPE ckmv0_matobj_tbl,
            lt_ckmlcr TYPE STANDARD TABLE OF ckmlcr.

      DATA: lv_bdatj_1 TYPE  ckmlpp-bdatj,
            lv_poper_1 TYPE  ckmlpp-poper.

      lv_bdatj_1 = _ano.
      lv_poper_1 = _mes.

      wa_kalnr-kalnr = _kalnr.
      APPEND wa_kalnr TO lt_kalnr.

      CALL FUNCTION 'CKMS_PERIOD_READ_WITH_ITAB'
        EXPORTING
          i_bdatj_1               = lv_bdatj_1
          i_poper_1               = lv_poper_1
        TABLES
          t_kalnr                 = lt_kalnr
          t_ckmlcr                = lt_ckmlcr
        EXCEPTIONS
          no_data_found           = 1
          input_data_inconsistent = 2
          buffer_inconsistent     = 3
          OTHERS                  = 4.

      DELETE lt_ckmlcr WHERE waers NE i_waers.

      IF lt_ckmlcr[] IS NOT INITIAL.
        READ TABLE lt_ckmlcr INTO DATA(wl_ckmlcr) INDEX 1.
* <--- S4 Migration - 06/07/2023 - DG

        CASE i_vprsv.
          WHEN 'S'. "Preço standard
            r_preco = wl_ckmlcr-stprs.
          WHEN 'V'. "Preço médio móvel/preço interno periódico.
            r_preco = wl_ckmlcr-pvprs.
        ENDCASE.

        _encontrou_preco = abap_true.

      ENDIF.

    ENDWHILE.

    IF ( r_preco <= 0 ) AND ( _encontrou_preco EQ abap_true ).

      RAISE EXCEPTION TYPE zcx_preco
        EXPORTING
          textid = VALUE #( msgid = zcx_preco=>zcx_erro_preco_estoque-msgid
                            msgno = zcx_preco=>zcx_erro_preco_estoque-msgno
                            attr1 = CONV #( 'Preço não definido'                         )
                            attr2 = CONV #( |Código Controle: { i_vprsv } / { i_waers } | )
                            attr3 = CONV #( |Ano : { wl_ckmlcr-bdatj    } |              )
                            attr4 = CONV #( |Mês:  { wl_ckmlcr-poper    } |              )
                           )
          msgty  = 'E'
          msgno  = zcx_preco=>zcx_erro_preco_estoque-msgno
          msgid  = zcx_preco=>zcx_erro_preco_estoque-msgid
          msgv1  = CONV #( 'Preço não definido' )
          msgv2  = CONV #( |Código Controle: { i_vprsv } / { i_waers } | )
          msgv3  = CONV #( |Ano : { wl_ckmlcr-bdatj    } | )
          msgv4  = CONV #( |Mês:  { wl_ckmlcr-poper    } | ).

    ELSEIF r_preco <= 0.

      RAISE EXCEPTION TYPE zcx_preco
        EXPORTING
          textid = VALUE #( msgid = zcx_preco=>zcx_erro_preco_estoque-msgid
                            msgno = zcx_preco=>zcx_erro_preco_estoque-msgno
                            attr1 = CONV #( 'Preço não encontrado'                        )
                            attr2 = CONV #( |Código Controle: { i_vprsv } / { i_waers } | )
                            attr3 = CONV #( |Material: { i_matnr  } | )
                            attr4 = CONV #( |Centro: { i_bwkey } | )
                           )
          msgty  = 'E'
          msgno  = zcx_preco=>zcx_erro_preco_estoque-msgno
          msgid  = zcx_preco=>zcx_erro_preco_estoque-msgid
          msgv1  = CONV #( 'Preço não encontrado' )
          msgv2  = CONV #( |Código Controle: { i_vprsv } / { i_waers } | )
          msgv3  = CONV #( |Material: { i_matnr  } | )
          msgv4  = CONV #( |Centro: { i_bwkey } | ).

    ENDIF.




  ENDMETHOD.


  method ZIF_PRECO~GET_PRECO_PAUTA.

    DATA: IT_A924          TYPE TABLE OF A924,
          IT_KONP          TYPE TABLE OF KONP.

    CLEAR: R_KBETR, IT_A924[], IT_KONP[].

    IF ( I_REGIO IS INITIAL ) OR ( I_MATNR IS INITIAL ) OR ( I_INCO1 IS INITIAL ).
      RAISE EXCEPTION TYPE ZCX_PRECO
         EXPORTING
           TEXTID = VALUE #( MSGID = ZCX_PRECO=>ZCX_ERRO_PRECO_PAUTA-MSGID
                             MSGNO = ZCX_PRECO=>ZCX_ERRO_PRECO_PAUTA-MSGNO
                             ATTR1 = CONV #( 'Parâmetros não informados' )
                            )
           MSGTY  = 'E'
           MSGNO  = ZCX_PRECO=>ZCX_ERRO_PRECO_PAUTA-MSGNO
           MSGID  = ZCX_PRECO=>ZCX_ERRO_PRECO_PAUTA-MSGID
           MSGV1 = CONV #( 'Parâmetros não informados' ).
    ENDIF.

    SELECT *
      FROM A924 AS A INTO TABLE IT_A924
     WHERE KAPPL      EQ 'V'     "SD
       AND KSCHL      EQ 'ZIVP'
       AND ALAND      EQ 'BR'
       AND TXREG_SF   EQ I_REGIO
       AND INCO1      EQ I_INCO1
       AND MATNR      EQ I_MATNR
       AND KFRST      EQ SPACE     "Liberado
       AND DATAB      LE SY-DATUM
       AND DATBI      GE SY-DATUM
       AND EXISTS ( SELECT *
                      FROM KONP
                     WHERE KNUMH    EQ A~KNUMH
                       AND LOEVM_KO EQ '' ).

    IF ( IT_A924[] IS INITIAL ) AND ( I_LAST EQ ABAP_TRUE ).

      SELECT *
        FROM A924 AS A INTO TABLE IT_A924
       WHERE KAPPL      EQ 'V'     "SD
         AND KSCHL      EQ 'ZIVP'
         AND ALAND      EQ 'BR'
         AND TXREG_SF   EQ I_REGIO
         AND INCO1      EQ I_INCO1
         AND MATNR      EQ I_MATNR
         AND KFRST      EQ SPACE     "Liberado

         AND EXISTS ( SELECT *
                        FROM KONP
                       WHERE KNUMH    EQ A~KNUMH
                         AND LOEVM_KO EQ '' )

         AND DATBI      EQ ( SELECT MAX( DATBI )
                               FROM A924 AS X
                              WHERE KAPPL      EQ 'V'     "SD
                                AND KSCHL      EQ 'ZIVP'
                                AND ALAND      EQ 'BR'
                                AND TXREG_SF   EQ I_REGIO
                                AND INCO1      EQ I_INCO1
                                AND MATNR      EQ I_MATNR
                                AND KFRST      EQ SPACE     "Liberado
                                AND EXISTS ( SELECT *
                                               FROM KONP
                                              WHERE KNUMH    EQ X~KNUMH
                                                AND LOEVM_KO EQ '' ) ).
    ENDIF.

    IF IT_A924[] IS NOT INITIAL.
      SELECT *
        FROM KONP APPENDING TABLE IT_KONP
         FOR ALL ENTRIES IN IT_A924
       WHERE KNUMH    EQ IT_A924-KNUMH
         AND LOEVM_KO EQ ''.
    ENDIF.

    DATA(_COUNT_FRETE) = 0.
    LOOP AT IT_A924 INTO DATA(WA_A924).
      READ TABLE IT_KONP INTO DATA(WA_KONP) WITH KEY KNUMH = WA_A924-KNUMH.
      IF ( SY-SUBRC = 0 ) AND ( WA_KONP-KPEIN EQ 1 ).
        CASE WA_KONP-KMEIN.
          WHEN 'KG'.
            R_KBETR = WA_KONP-KBETR.
            e_konp = wa_konp.
            ADD 1 TO _COUNT_FRETE.
          WHEN 'TO'.
            R_KBETR = WA_KONP-KBETR / 1000.
            e_konp = wa_konp.
            ADD 1 TO _COUNT_FRETE.
        ENDCASE.
      ENDIF.
    ENDLOOP.

    IF _COUNT_FRETE EQ 0.
      RAISE EXCEPTION TYPE ZCX_PRECO
         EXPORTING
           TEXTID = VALUE #( MSGID = ZCX_PRECO=>ZCX_ERRO_PRECO_PAUTA-MSGID
                             MSGNO = ZCX_PRECO=>ZCX_ERRO_PRECO_PAUTA-MSGNO
                             ATTR1 = CONV #( 'Nenhum cadastro encontrado!' )
                            )
           MSGTY  = 'E'
           MSGNO  = ZCX_PRECO=>ZCX_ERRO_PRECO_PAUTA-MSGNO
           MSGID  = ZCX_PRECO=>ZCX_ERRO_PRECO_PAUTA-MSGID
           MSGV1 = CONV #( 'Nenhum cadastro encontrado!' ).
    ELSEIF _COUNT_FRETE > 1.
      RAISE EXCEPTION TYPE ZCX_PRECO
         EXPORTING
           TEXTID = VALUE #( MSGID = ZCX_PRECO=>ZCX_ERRO_PRECO_PAUTA-MSGID
                             MSGNO = ZCX_PRECO=>ZCX_ERRO_PRECO_PAUTA-MSGNO
                             ATTR1 = CONV #( 'Mais de um cadastro encontrado!' )
                            )
           MSGTY  = 'E'
           MSGNO  = ZCX_PRECO=>ZCX_ERRO_PRECO_PAUTA-MSGNO
           MSGID  = ZCX_PRECO=>ZCX_ERRO_PRECO_PAUTA-MSGID
           MSGV1 = CONV #( 'Mais de um cadastro encontrado!' ).
    ELSEIF R_KBETR <= 0.
      RAISE EXCEPTION TYPE ZCX_PRECO
         EXPORTING
           TEXTID = VALUE #( MSGID = ZCX_PRECO=>ZCX_ERRO_PRECO_PAUTA-MSGID
                             MSGNO = ZCX_PRECO=>ZCX_ERRO_PRECO_PAUTA-MSGNO
                             ATTR1 = CONV #( 'Preço em KG não encontrado!' )
                            )
           MSGTY  = 'E'
           MSGNO  = ZCX_PRECO=>ZCX_ERRO_PRECO_PAUTA-MSGNO
           MSGID  = ZCX_PRECO=>ZCX_ERRO_PRECO_PAUTA-MSGID
           MSGV1 = CONV #( 'Preço em KG não encontrado!' ).
    ENDIF.



  endmethod.
ENDCLASS.

FUNCTION z_modify_condition_price.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      T_VTTP STRUCTURE  VTTP
*"  CHANGING
*"     REFERENCE(C_XKOMV) TYPE  KOMV_INDEX
*"----------------------------------------------------------------------

  FIELD-SYMBOLS: <field> TYPE any.

  "Internal Table e Work Area
  DATA: wa_vttp     TYPE vttp,
        ti_lips     TYPE TABLE OF lips,
        ti_vtpa     TYPE TABLE OF vtpa,
        wa_lips     TYPE lips,
        wa_vttk     TYPE vttk,
        wa_vtpa     TYPE vtpa,
        wa_vttk_aux TYPE vttk.

  DATA: v_vbeln         TYPE  vbeln,
        v_placa_cav     TYPE  zplaca,
        v_vlr_frete_neg TYPE  zvalor_frete,
        v_lifnr_pc      TYPE  vtpa-lifnr,
        v_lifnr_sp      TYPE  vtpa-lifnr,
        v_kunna         TYPE  vtts-kunna,
        v_kunnz         TYPE  vtts-kunnz,
        v_lifnz         TYPE  vtts-lifnz,
        v_lzone_kunna   TYPE  kna1-lzone,
        v_lzone_kunnz   TYPE  kna1-lzone,
        v_operacao      TYPE  zlest0060-operacao,
        v_adrnr_pc      TYPE  vtpa-adrnr,
        v_adrnr_lr      TYPE  vtpa-adrnr,
        v_tzone_pc      TYPE  adrc-transpzone,
        v_tzone_lr      TYPE  adrc-transpzone,
        v_route         TYPE  trolz-route,
        v_kbetr         TYPE  komv-kbetr.

  CLEAR: wa_vttk, wa_vttk_aux, ti_vtpa[], ti_lips[],
         v_kunna, v_kunnz, v_operacao,
         v_lzone_kunna, v_lzone_kunnz, v_lifnz,
         v_adrnr_pc, v_adrnr_lr,
         v_tzone_pc, v_tzone_lr,
         v_route, v_kbetr.

  DATA: status TYPE syst_subrc.

  READ TABLE t_vttp INTO wa_vttp INDEX 1.

  CHECK sy-subrc = 0.

  SELECT SINGLE *
    FROM vttk INTO wa_vttk
   WHERE tknum EQ wa_vttp-tknum.

  CHECK sy-subrc = 0.

  SELECT *
    FROM vtpa INTO TABLE ti_vtpa
   WHERE vbeln EQ wa_vttp-tknum.

  ASSIGN ('(SAPMV56A)VTTK') TO <field>.

  IF <field> IS ASSIGNED.
    MOVE-CORRESPONDING: <field> TO wa_vttk_aux .
  ENDIF.

  IF wa_vttk-shtyp IS INITIAL.
    MOVE wa_vttk_aux-shtyp TO wa_vttk-shtyp.
  ENDIF.

  IF wa_vttk-tdlnr IS INITIAL.
    MOVE wa_vttk_aux-tdlnr TO wa_vttk-tdlnr.
  ENDIF.

  IF wa_vttk-route IS INITIAL.
    MOVE wa_vttk_aux-route TO wa_vttk-route.
  ENDIF.

  IF wa_vttk-sdabw IS INITIAL.
    MOVE wa_vttk_aux-sdabw TO wa_vttk-sdabw.
  ENDIF.

  SELECT *
    FROM lips INTO TABLE ti_lips
     FOR ALL ENTRIES IN t_vttp
   WHERE vbeln EQ t_vttp-vbeln.

  CHECK ti_lips[] IS NOT INITIAL.

  LOOP AT ti_lips INTO wa_lips.

    CASE c_xkomv-kschl.

      WHEN 'ZVCT'.

        DATA: wa_zlest0026_anterior TYPE zlest0026.

        CALL FUNCTION 'Z_CK_TRIAGEM_MESMO_VEICULO'
          EXPORTING
            i_tknum               = wa_vttk-tknum
            i_text1               = wa_vttk-text1
            i_vbeln               = wa_lips-vbeln
          IMPORTING
            wa_zlest0026_anterior = wa_zlest0026_anterior.

        IF wa_zlest0026_anterior IS NOT INITIAL.
          c_xkomv-kbetr = 0.
        ENDIF.

        UPDATE zlest0026
           SET vlr_triagem = c_xkomv-kbetr
         WHERE tknum EQ wa_vttk-tknum.

      WHEN 'ZFRE'.

*-----------------------------------------------------------------------------------------------------------------*
*       Checar Preço Ferroviario
*-------------------------------------------------------------------------------------------*

        IF ( wa_vttk-vsart EQ '02'   ) AND  "Ferroviario
           ( c_xkomv-kbetr EQ '0.01' ).

          SELECT SINGLE *
            FROM a934 INTO @DATA(wl_a934)
           WHERE knumh EQ @c_xkomv-knumh.

          IF sy-subrc IS NOT INITIAL.
            SELECT SINGLE *
              FROM a938 INTO @DATA(wl_a938)
             WHERE knumh EQ @c_xkomv-knumh.

            IF sy-subrc IS INITIAL.
              wl_a934-lzonea = wl_a938-lzonea.
              wl_a934-lzonez = wl_a938-lzonez.
            ENDIF.
          ENDIF.

          IF ( sy-subrc EQ 0 ).

            c_xkomv-kbetr = 0.

            CLEAR: v_lifnr_pc, v_lifnr_sp.

            READ TABLE ti_vtpa INTO DATA(wl_vtpa) WITH KEY parvw = 'PC'.
            IF sy-subrc EQ 0.
              v_lifnr_pc = wl_vtpa-lifnr.
            ENDIF.

            READ TABLE ti_vtpa INTO wl_vtpa WITH KEY parvw = 'SP'.
            IF sy-subrc EQ 0.
              v_lifnr_sp = wl_vtpa-lifnr.
            ENDIF.

            TRY.
                zcl_calc_frete=>get_cidade_tabela_mesorregiao(
                   EXPORTING
                     i_agente_frete   =  CONV #( v_lifnr_sp )
                     i_ponto_coleta   =  CONV #( v_lifnr_pc )
                   RECEIVING
                     r_id_cidade_base =  DATA(_id_cidade_base)   ).

                CHECK _id_cidade_base IS NOT INITIAL.

                zcl_calc_frete=>get_valor_frete(
                  EXPORTING
                    i_tdlnr           =  CONV #( v_lifnr_sp )
                    i_shtyp           =  CONV #( wa_vttk-shtyp )
                    i_lzonea          =  CONV #( wl_a934-lzonea )
                    i_lzonez          =  CONV #( wl_a934-lzonez )
                    i_id_cidade_base  =  CONV #( _id_cidade_base )
                    i_data_referencia =  CONV #( c_xkomv-kdatu )
                    i_matnr           =  wa_lips-matnr
                  IMPORTING
                    e_kbetr           =  DATA(_kbetr) ).

                c_xkomv-kbetr = _kbetr.

              CATCH zcx_calc_frete.
            ENDTRY.

            EXIT.

          ENDIF.  "SELECT SINGLE * FROM A934
        ENDIF. " IF ( WA_VTTK-VSART EQ '02'   ) AND  "Ferroviario

*-----------------------------------------------------------------------------------------------------------------*
*      Checar alteração de preço de Frete para a Ordem Carregamento/Ordem de Venda - Transação ZLES0153
*      Buscar Preço definido na Viagem do CARGUERO
*-----------------------------------------------------------------------------------------------------------------*

        IF wa_vttk-id_ordem IS NOT INITIAL AND
           strlen( wa_vttk-text1 ) >= 7.

          v_placa_cav     = wa_vttk-text1(7).

          CALL FUNCTION 'ZLES_VALOR_FRETE_ORDEM_CAR'
            EXPORTING
              i_id_ordem      = wa_vttk-id_ordem
              i_placa_cav     = v_placa_cav
              i_shtyp         = wa_vttk-shtyp
            IMPORTING
              e_vlr_frete_neg = v_vlr_frete_neg.

          CHECK v_vlr_frete_neg > 0.

          c_xkomv-kbetr = v_vlr_frete_neg. "Atribuir Valor de Frete Negociado

        ELSEIF ( wa_lips-vgbel IS NOT INITIAL ) AND  "Ordem Venda
               ( wa_vttk-text1 IS NOT INITIAL ) AND
               ( strlen( wa_vttk-text1 ) >= 7 ). "Placa Cavalo

          v_vbeln         = wa_lips-vgbel.
          v_placa_cav     = wa_vttk-text1(7).

          CALL FUNCTION 'ZLES_VALOR_FRETE_ORDEM_CAR'
            EXPORTING
              i_vbeln         = v_vbeln
              i_placa_cav     = v_placa_cav
              i_id_ordem      = wa_vttk-id_ordem
              i_shtyp         = wa_vttk-shtyp
            IMPORTING
              e_vlr_frete_neg = v_vlr_frete_neg.

          IF v_vlr_frete_neg > 0.
            c_xkomv-kbetr = v_vlr_frete_neg. "Atribuir Valor de Frete Negociado
            CONTINUE.
          ENDIF.

        ENDIF.

*-------CS2020000700 - jtassoni - 21.09.2020 - inicio
*-------------------------------------------
*------ Aquaviarios
*-------------------------------------------
        IF wa_vttk-vsart = '03'.

          SELECT    kunna    kunnz    lifnz
            INTO (v_kunna, v_kunnz, v_lifnz)
            FROM vtts
              UP TO 1 ROWS
           WHERE tknum = wa_vttp-tknum.
          ENDSELECT.

          IF v_kunnz IS INITIAL.
            v_kunnz = v_lifnz.
          ENDIF.

          SELECT lzone
            INTO v_lzone_kunna
            FROM kna1
              UP TO 1 ROWS
           WHERE kunnr = v_kunna.
          ENDSELECT.

          SELECT lzone
            INTO v_lzone_kunnz
            FROM kna1
              UP TO 1 ROWS
           WHERE kunnr = v_kunnz.
          ENDSELECT.

*-------------------------------------------
*---------Seleção para tipo de operação
*-------------------------------------------
          SELECT operacao
            INTO v_operacao
            FROM zlest0060
              UP TO 1 ROWS
           WHERE doc_rem = wa_vttp-vbeln.
          ENDSELECT.

          IF v_operacao IS INITIAL.
            SELECT operacao INTO v_operacao
              FROM zlest0060
                UP TO 1 ROWS
             WHERE vbeln_aviso_ref = wa_vttp-vbeln.
            ENDSELECT.
          ENDIF.

*-------------------------------------------
*---------Seleção para Itinerário
*-------------------------------------------
          READ TABLE ti_vtpa INTO wa_vtpa WITH KEY parvw = 'PC'.
          IF sy-subrc = 0.
            SELECT transpzone
              INTO v_tzone_pc
              FROM adrc
                UP TO 1 ROWS
             WHERE addrnumber = wa_vtpa-adrnr.
            ENDSELECT.
          ENDIF.

          READ TABLE ti_vtpa INTO wa_vtpa WITH KEY parvw = 'LR'.
          IF sy-subrc = 0.
            SELECT transpzone
              INTO v_tzone_lr
              FROM adrc
                UP TO 1 ROWS
             WHERE addrnumber = wa_vtpa-adrnr.
            ENDSELECT.
          ENDIF.

          SELECT route
            INTO v_route
            FROM trolz
              UP TO 1 ROWS
           WHERE aland = 'BR'
             AND azone = v_tzone_pc
             AND lzone = v_tzone_lr.
          ENDSELECT.

*-----------------------------------------
*---------busca novo valor frete
*-----------------------------------------
          IF wa_vttk-vsart = '03'   AND
             wa_vttk-shtyp = 'Z002' AND
             c_xkomv-kbetr = '1.00'.
            TRY.
                CLEAR c_xkomv-kbetr.

                zcl_calc_frete=>get_valor_frete(
                  EXPORTING
                    i_tdlnr           =  CONV #( wa_vttk-tdlnr )
                    i_shtyp           =  CONV #( wa_vttk-shtyp )
                    i_lzonea          =  CONV #( v_lzone_kunna )
                    i_lzonez          =  CONV #( v_lzone_kunnz )
                    i_vegr5           =  CONV #( v_operacao )
                    i_data_referencia =  CONV #( wa_vttk-erdat )
                  IMPORTING
                    e_kbetr           =  DATA(_kbetr2) ).

                c_xkomv-kbetr = _kbetr2.
                c_xkomv-kwert = ( _kbetr2 * 100 ) / 1000.
              CATCH zcx_calc_frete.
            ENDTRY.
          ENDIF.

          IF wa_vttk-vsart = '03'   AND
             wa_vttk-shtyp = 'Z027' AND
             c_xkomv-kbetr = '1.00'.
            TRY.
                CLEAR c_xkomv-kbetr.

                zcl_calc_frete=>get_valor_frete(
                  EXPORTING
                    i_tdlnr           =  CONV #( wa_vttk-tdlnr )
                    i_shtyp           =  CONV #( wa_vttk-shtyp )
                    i_lzonea          =  CONV #( v_lzone_kunna )
                    i_lzonez          =  CONV #( v_lzone_kunnz )
                    i_route           =  CONV #( v_route )
                    i_vegr5           =  CONV #( v_operacao )
                    i_data_referencia =  CONV #( wa_vttk-erdat )
                  IMPORTING
                    e_kbetr           =  DATA(_kbetr3) ).

                c_xkomv-kbetr = _kbetr3.
                c_xkomv-kwert = ( _kbetr3 * 100 ) / 1000.
              CATCH zcx_calc_frete.
            ENDTRY.
          ENDIF.
        ENDIF.
*-------CS2020000700 - jtassoni - 21.09.2020 - inicio


        "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP --->>>
*        IF wa_vttk-vsart = '01'. "Rodoviario.
*          SELECT SINGLE id_interface, nro_cg
*            FROM zsdt0001 INTO @DATA(lwa_zsdt0001)
*            WHERE doc_rem EQ @wa_lips-vbeln.
*
*          IF sy-subrc EQ 0 AND lwa_zsdt0001-id_interface = '48' AND lwa_zsdt0001-nro_cg IS NOT INITIAL.
*
*            zcl_carga_saida_insumos=>busca_dados_carga(
*              EXPORTING
*                i_nr_carga_single        = CONV #( lwa_zsdt0001-nro_cg )
*              IMPORTING
*                e_romaneios              = DATA(lit_romaneios) ).
*
*            READ TABLE lit_romaneios INTO DATA(lwa_romaneio) WITH KEY  doc_rem = wa_lips-vbeln.
*            IF sy-subrc EQ 0.
*
*              IF lwa_romaneio-itens[] IS NOT INITIAL.
*                READ TABLE lwa_romaneio-itens INTO DATA(lwa_romaneio_item) WITH KEY matnr = wa_lips-matnr
*                                                                                    charg = wa_lips-charg.
*                IF sy-subrc EQ 0.
*                  c_xkomv-kbetr = lwa_romaneio_item-preco_zfre.
*                ENDIF.
*              ELSE.
*                c_xkomv-kbetr = lwa_romaneio-preco_zfre.
*              ENDIF.
*
*              CONTINUE.
*            ENDIF.
*          ENDIF.
*        ENDIF.
        "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP <<----

**=============================US #66690 / AOENNING / 21-06-2023
      WHEN 'ZSEG' OR 'ZIOF'.

        CLEAR: v_placa_cav.
        v_placa_cav     = wa_vttk-text1(7).

        IF v_placa_cav IS NOT INITIAL. "Validar se a placa esta preenchida.
          IF wa_vttk-vsart EQ '01' OR wa_vttk-vsart EQ '07'. "01 - Rodoviario / 07 - Multimodal

            "Verificar ...
            CALL FUNCTION 'Z_LES_EXC_ZSEG'
              EXPORTING
                i_placa       = v_placa_cav
                i_ck_consulta = abap_true
              IMPORTING
                e_status      = status.

            CASE status.
              WHEN 0. "TAG "CTC e "ETC equiparado."Não deve validar zseg, e devemos zera-la
                c_xkomv-kbetr = ''.
              WHEN 1. "Erro de comunicação com a API
                c_xkomv-kbetr = ''.
              WHEN 2. "ETC Não equiparado
              WHEN 3. "Tip não retornou o resultado esperado na consulta
                c_xkomv-kbetr = ''.
              WHEN OTHERS.
            ENDCASE.
          ENDIF.
        ENDIF.
      WHEN OTHERS.

**==================================US #66690 / AOENNING / 21-06-2023
    ENDCASE.

  ENDLOOP.

ENDFUNCTION.

class ZCL_TAXA_CURVA definition
  public
  create public .

*"* public components of class ZCL_TAXA_CURVA
*"* do not include other source files here!!!
public section.

*---> 20.06.2023 - Migração S4 - DG
"      !I_TIPO type CHAR02 optional .
  methods SET_NUMERO
    importing
      !I_NUMERO type ZSDED013
      !I_TIPO type ZCHAR02 optional .
*<--- 20.06.2023 - Migração S4 - DG
  methods SET_DATA_VENC
    importing
      !I_DATA_VENC type VALDT
    returning
      value(E_DATA) type DATUM .
  methods SET_DATA_LIB
    importing
      !I_DATA_LIB type DATUM .
  methods SET_CADENCIA
    importing
      !I_CADENCIA type DZMENG
      !I_NEGATIVA type CHAR01 optional
      !I_TCODE type SYTCODE optional .
  methods SET_CAD_IN_DENTREGA
    importing
      !I_CADENCIA type DZMENG .
  methods SET_ZIEME
    importing
      !I_ZIEME type DZIEME .
  methods SET_TOTAL_PROPORCIONAL
    importing
      !I_TOTAL type DMBTR
      !I_NEGATIVA type CHAR01 optional .
  methods SET_TAXA_CURVA
    importing
      !I_TAXA type KURRF .
  methods GET_NUMERO
    returning
      value(E_NUMERO) type ZSDED013 .
  methods GET_DATA_VENC
    returning
      value(E_DATA_VENC) type VALDT .
  methods GET_DATA_LIB
    returning
      value(E_DATA_LIB) type DATUM .
  methods GET_CADENCIA
    returning
      value(E_CADENCIA) type DZMENG .
  methods GET_ZIEME
    returning
      value(E_ZIEME) type DZIEME .
  methods GET_TOTAL_PROPORCIONAL
    returning
      value(E_TOTAL) type DMBTR .
  methods GET_TAXA_CURVA
    returning
      value(E_TAXA) type KURRF .
  methods GET_FRETE_CIF
    returning
      value(E_VALOR) type BEZEI60 .
  methods SET_FRETE_PORTO
    importing
      !I_VALOR type BEZEI60 .
  methods SET_FRETE_CIF
    importing
      !I_VALOR type BEZEI60 .
  methods GET_FRETE_PORTO
    returning
      value(E_VALOR) type BEZEI60 .
  methods SET_FOBS
    importing
      !I_VALOR type BEZEI60 .
  methods GET_FOBS
    returning
      value(E_VALOR) type BEZEI60 .
  methods SET_TIPO
    importing
      !I_TIPO type CHAR03 .
  methods GET_TIPO
    returning
      value(E_TIPO) type CHAR03 .
  methods SET_POSNR
    importing
      !I_POSNR type POSNR .
  methods GET_BEZEI
    returning
      value(E_BEZEI) type BEZEI30 .
  methods GET_POSNR
    returning
      value(E_POSNR) type POSNR .
  methods SET_BEZEI
    importing
      !I_BEZEI type BEZEI30 .
  methods SET_ESTORNO
    importing
      !I_NUMERO type NUM10 .
  methods GET_ESTORNO
    returning
      value(E_RETORNO) type NUM10 .
  methods SET_DATA_REGISTRO
    importing
      !I_DATA type SYDATUM .
  methods GET_DATA_REGISTRO
    returning
      value(E_DATA) type SYDATUM .
  methods SET_HORA_REGISTRO
    importing
      !I_HORA type SYUZEIT .
  methods GET_HORA_REGISTRO
    returning
      value(E_HORA) type SYUZEIT .
  methods SET_TAXA_CAMBIO
    importing
      !I_TAXA type UKURSP
      !I_DATA type DATUM optional
      !I_NUMERO type ZSDED013 optional
      !I_TIPO type CHAR3 optional
      !I_ZERAR type CHAR1 optional .
  methods GET_TAXA_CAMBIO
    returning
      value(E_TAXA) type UKURSP .
  methods SET_TIPO_TAXA
    importing
      !I_TIPO type CHAR01 .
  methods GET_TIPO_TAXA
    returning
      value(E_TIPO) type CHAR01 .
  methods VERIFICA_TIPO_TAXA
    importing
      !I_TCODE type SYTCODE optional
      !I_TIPO type CHAR03 optional
      !I_STATUS type CHAR01 optional
    preferred parameter I_TCODE .
  methods SET_VBELN
    importing
      !I_VBELN type VBELN .
  methods GET_VBELN
    returning
      value(E_VBELN) type VBELN .
  methods ESTORNO_VF11
    importing
      !I_ZSDT0094 type ZSDT0094
      !I_AUART type AUART
      !I_TCODE type SY-TCODE
      !I_VBELN type VBELN .
  methods SET_INCOTERMS
    importing
      !I_INCO1 type INCO1 .
  methods GET_INCOTERMS
    returning
      value(E_INCO1) type INCO1 .
  methods SET_SAFRA
    importing
      !I_SAFRA type AJAHR optional .
  methods GET_SAFRA
    returning
      value(E_SAFRA) type AJAHR .
  methods AGRUPA_DADOS
    importing
      !I_NUMERO type ZSDED003
      !T_ITENS type STANDARD TABLE optional
      !I_TIPO type CHAR3 optional
    exporting
      value(I_0041) type STANDARD TABLE
      !E_HEDGE type XFELD .
  methods CALC_TOT
    importing
      !I_ZIEME type DZIEME optional
      !I_TAXA type KURSF optional
    returning
      value(I_VALOR) type DMBTR .
  methods SET_CADENCIA_IN
    importing
      !I_CADENCIA type DZMENG
      !I_NEGATIVA type CHAR1 optional
      !I_0040 type ZSDT0040 optional
      !I_ESTORNO type CHAR1 optional
      !I_TIPO type CHAR3 optional .
  methods SET_FRETE_IN
    importing
      !I_FRETE type KBETR optional
      !I_ZIEME type DZIEME optional .
  methods GET_FRETE_IN
    returning
      value(E_FRETE) type KBETR .
  methods SET_MATKL
    importing
      !I_MATKL type MATKL optional
      !I_BRGEW type BRGEW optional .
  methods GET_MATKL
    returning
      value(E_MATKL) type MATKL .
  methods SET_NETPR
    importing
      !I_NETPR type NETPR optional
      !I_KMEIN type KMEIN optional .
  methods GET_NETPR
    returning
      value(E_NETPR) type NETPR .
  methods TIPO_TAXA_IN
    importing
      !I_TIPO type CHAR3 optional .
  methods GET_TAXA_IN
    returning
      value(E_TAXA) type UKURSP .
  methods SET_TAXA_IN
    importing
      !I_VENC type DATUM optional .
  methods GET_TABLES
    importing
      !I_VBELN type VBELN optional
    returning
      value(RETURN) type SY-SUBRC .
  methods GET_FIRTS_FRETE
    importing
      !I_VBELN type VBELN optional
      !I_MATNR type MATNR optional
      !I_DOC_SIMULACAO type ZSDED003 optional
    returning
      value(W_0041) type ZSDT0041 .
  methods GET_AUART
    importing
      !SET type SETNAMENEW optional
    returning
      value(R_RANGE) type ZRSDSSELOPTS .
  methods CHECK_AUART
    importing
      !TIPO type CHAR3
      !NUMERO type ZSDED013
    returning
      value(RETURN) type SY-SUBRC .
  methods SET_TX_CAMBIO_AQV
    importing
      !I_TAXA type UKURSP
      !I_DATA type DATUM
      !I_VBELN type VBELN
      !I_TIPO type CHAR3
      !I_ZERAR type CHAR1 optional .
  methods GET_TIPO_AUART
    importing
      !VBRK type VBRK
      !AUART type AUART optional
    returning
      value(RETURN) type CHAR3 .
  methods SET_INTERCOMPANY
    importing
      !S_ICPANY type CHAR01 .
  methods GET_INTERCOMPANY
    returning
      value(E_ICPANY) type CHAR01 .
  methods SET_TX_CAMBIO_IN
    importing
      !I_TAXA type UKURSP
      !I_TIPO type CHAR3
      !I_ZERAR type CHAR1 optional
      !I_NUMERO type ZSDED013 optional .
  methods SET_TABLES
    importing
      !INPUT type SY-SUBRC .
  methods SET_TAXA_CAMBIO_BOLETA
    importing
      !I_NUMERO type ZSDED013 optional .
protected section.
*"* protected components of class ZCL_TAXA_CURVA
*"* do not include other source files here!!!
private section.

*"* private components of class ZCL_TAXA_CURVA
*"* do not include other source files here!!!
  data AT_NUMERO type ZSDED013 .
  data AT_DATA_VENC type VALDT .
  data AT_DATA_LIB type DATUM .
  data AT_CADENCIA type DZMENG .
  data AT_ZIEME type DZIEME .
  data AT_TOTAL_PROPORCIONAL type DMBTR .
  data AT_TAXA_CURVA type KURRF .
  data AT_FRETE_CIF type BEZEI60 .
  data AT_FRETE_PORTO type BEZEI60 .
  data AT_FOBS type BEZEI60 .
  data AT_TIPO type CHAR03 .
  data AT_POSNR type POSNR .
  data AT_BEZEI type BEZEI30 .
  data AT_ESTORNO type NUM10 .
  data AT_DATA_REGISTRO type SYDATUM .
  data AT_HORA_REGISTRO type SYUZEIT .
  data AT_TAXA_CAMBIO type UKURSP .
  data AT_TIPO_TAXA type CHAR01 .
  data AT_VBELN type VBELN .
  data AT_INCO1 type INCO1 .
  data AT_MATKL type MATKL .
  data AT_BRGEW type BRGEW .
  data AT_NETPR type NETPR .
  data AT_KMEIN type KMEIN .
  data AT_FRETE type KBETR .
  data AT_SAFRA type AJAHR .
  data AT_TAXA type UKURSP .
  data AT_CHECK type SY-SUBRC .
  data AT_ICPANY type CHAR01 .
ENDCLASS.



CLASS ZCL_TAXA_CURVA IMPLEMENTATION.


  METHOD agrupa_dados.


********************************************************************************
* Metodo para Agrupar os valores de quantidade, preços, taxas etc
********************************************************************************

******************************************************************************
**** TABELA ZSDT0041 COM MATKL PARA AGRUPAR OS DADOS PELO GRUPO DE MERCADORIA
******************************************************************************
    TYPES BEGIN OF ty_0041.
    INCLUDE TYPE zsdt0041.
    TYPES matkl TYPE matkl.
    TYPES brgew TYPE brgew.
*---> 05/07/2023 - Migração S4 - Inicio - AF.
*   TYPES DTPGTCULT TYPE BAPI_JBD_DTE_DZTERM.
    TYPES dtpgtcult TYPE tb_dzterm.
*---> 05/07/2023 - Migração S4 - Fim - AF.
    TYPES kursk TYPE kursk.
    TYPES END OF ty_0041.

************************************************************
**** TABELA PARA AGRUPAR OS VALORES POR GRUPO DE MERCADORIA
************************************************************
    TYPES: BEGIN OF ty_matkl,
             matkl         TYPE matkl,
             zmeng         TYPE dzmeng,
             desc_absoluto TYPE kbetr,
             vlrtot        TYPE zsded005,
             vlr_frete     TYPE kbetr,
             vlr_ajuste    TYPE kbetr,
           END OF ty_matkl.

***************************************************
**** OBJ DA TAXA CAMBIO E CURVA E OS METODOS DA 94
***************************************************
    DATA: obj_0094     TYPE REF TO zcl_taxa_curva.

**************************************
****  TABELAS INTERNAS E WORK AREAS
**************************************
    DATA:
      it_0041     TYPE TABLE OF ty_0041,
      it_zsdt0041 TYPE TABLE OF zsdt0041,
      it_itens    TYPE TABLE OF zsdt0041,
      r_matkl     TYPE TABLE OF ty_matkl,
      s_matkl     TYPE TABLE OF ty_matkl,
      l_matkl     LIKE LINE OF r_matkl,
      r_inco1     TYPE RANGE OF inco1,
      l_inco1     LIKE LINE OF r_inco1,
      wa_itens    TYPE zsdt0041.

**********************
****** LIBERA OS OBJ
**********************
    FREE: obj_0094.

**********************
****** CRIA OS OBJ
**********************
    CREATE OBJECT: obj_0094.

******************************
****** CABEÇARIO DA SIMULAÇÃO
******************************
    SELECT SINGLE * FROM zsdt0040
      INTO @DATA(wa_zsdt0040)
      WHERE doc_simulacao EQ @i_numero.

    wa_zsdt0040-dt_entrega_def = wa_zsdt0040-dt_entrega_def + 30.
    wa_zsdt0040-dt_entrega_sem = wa_zsdt0040-dt_entrega_sem + 30.
    wa_zsdt0040-dt_entrega_fet = wa_zsdt0040-dt_entrega_fet + 30.

*******************************************
****** BUSCA TAXA TRAVADA PARA O CABEÇARIO
*******************************************
    SELECT SINGLE * FROM zsdt0117
      INTO @DATA(wa_zsdt0117)
      WHERE bukrs EQ @wa_zsdt0040-vkorg
      AND desativado EQ @abap_false.

******************************
****** RANGE DE FRETE
******************************
    CASE i_tipo.
      WHEN 'FRI'.

        l_inco1-sign   = 'I'.
        l_inco1-option = 'EQ'.

        l_inco1-low    = 'CPT'.
        APPEND l_inco1 TO r_inco1.

        l_inco1-low    = 'CIF'.
        APPEND l_inco1 TO r_inco1.

        l_inco1-low    = 'CFR'.
        APPEND l_inco1 TO r_inco1.

    ENDCASE.

******************************
****** ITENS DA SIMULAÇÃO
******************************
    IF NOT t_itens IS INITIAL.
** Processo de Desconto ABS

      LOOP AT t_itens INTO wa_itens.
        APPEND wa_itens TO it_itens.
      ENDLOOP.

      SELECT * FROM zsdt0041
        INTO TABLE it_zsdt0041
        FOR ALL ENTRIES IN it_itens
        WHERE doc_simulacao EQ it_itens-doc_simulacao
          AND posnr         EQ it_itens-posnr
          AND vbeln         EQ it_itens-vbeln
          AND inco1         IN r_inco1.

      LOOP AT it_zsdt0041 ASSIGNING FIELD-SYMBOL(<t0041>).
        <t0041>-desc_absoluto = it_itens[ doc_simulacao = <t0041>-doc_simulacao
                                          posnr         = <t0041>-posnr
                                          vbeln         = <t0041>-vbeln ]-desc_absoluto.
      ENDLOOP.

    ELSE.

      SELECT * FROM zsdt0041
        INTO TABLE it_zsdt0041
        WHERE doc_simulacao EQ wa_zsdt0040-doc_simulacao
          AND inco1         IN r_inco1.

    ENDIF.

    IF i_tipo EQ 'FRI' .
**Inicio IR246304 - Ins Setor atividade para Iconterms CPT
*      DELETE IT_ZSDT0041 WHERE INCO1 EQ 'CPT'
*                           AND SPART NE '03'.
      DELETE it_zsdt0041 WHERE inco1 EQ 'CPT'
                           AND spart NE '03'
                           AND spart NE '02'
                           AND spart NE '04'.
****Fim IR246304 - Ins Setor atividade para Iconterms CPT
      DELETE it_zsdt0041 WHERE inco1 EQ 'CFR'
                          AND spart NE '03'.
    ENDIF.

    CHECK NOT it_zsdt0041 IS INITIAL.

******************************
****** BUSCA O MATKL NA MARA
******************************
    SELECT * FROM mara
      INTO TABLE @DATA(it_mara)
      FOR ALL ENTRIES IN @it_zsdt0041
      WHERE matnr EQ @it_zsdt0041-matnr.

* converter is valores
    it_0041 = it_zsdt0041.
    LOOP AT it_0041 ASSIGNING FIELD-SYMBOL(<f0041>).
** Inicio IR246304 - Ins Setor atividade  'CPT'
      IF <f0041>-inco1 EQ 'CPT'
      AND ( <f0041>-spart EQ '02'
      OR    <f0041>-spart EQ '03'
      OR    <f0041>-spart EQ '04' ).
        e_hedge = abap_true.
      ENDIF.
** Fim IR246304 - Ins Setor atividade  'CPT'


      DATA(wa_mara)         = it_mara[ matnr = <f0041>-matnr ].
      <f0041>-matkl         = wa_mara-matkl.
      <f0041>-brgew         = wa_mara-brgew.
      <f0041>-dtpgtcult     = wa_zsdt0040-dtpgtcult.

      <f0041>-kursk         = wa_zsdt0040-taxa_frete.
      IF <f0041>-kursk IS INITIAL.
        <f0041>-kursk         = wa_zsdt0117-kursk.
      ENDIF.

      obj_0094->set_matkl( i_matkl = wa_mara-matkl
                           i_brgew = wa_mara-brgew
                         ).
      obj_0094->set_zieme( <f0041>-zieme ).
      obj_0094->set_cadencia_in( <f0041>-zmeng ).
      obj_0094->set_zieme( <f0041>-zieme ).
      obj_0094->set_frete_in( i_frete = <f0041>-vlr_frete ).

      <f0041>-zmeng         = obj_0094->get_cadencia( ).

      l_matkl-matkl         = wa_mara-matkl.
      l_matkl-zmeng         = <f0041>-zmeng.
      l_matkl-desc_absoluto = <f0041>-desc_absoluto.

      IF i_tipo EQ 'VDI'.
        l_matkl-vlrtot        = <f0041>-vlrtot.
      ELSE.
        IF <f0041>-spart EQ '03'.
          l_matkl-vlrtot        = <f0041>-vlrtot.
        ELSE.
          l_matkl-vlrtot        = obj_0094->get_total_proporcional( ).
        ENDIF.

      ENDIF.

      l_matkl-vlr_frete     = obj_0094->get_frete_in( ).

      l_matkl-vlr_ajuste = <f0041>-vlr_ajuste.

      CLEAR <f0041>-inco1.

      APPEND l_matkl TO r_matkl.

    ENDLOOP.

* agrupa os dados
    LOOP AT r_matkl INTO l_matkl.
      COLLECT l_matkl INTO s_matkl.
    ENDLOOP.

* ordena e deleta duplicidade
    SORT it_0041 BY matkl.
    DELETE ADJACENT DUPLICATES FROM it_0041 COMPARING matkl.

* altera os valores de cada grupo de mercadoria com os valores agrupados da quantidade, desconto, total proporcional
    LOOP AT it_0041 ASSIGNING <f0041>.
      <f0041>-zmeng         = s_matkl[ matkl = <f0041>-matkl ]-zmeng.
      <f0041>-desc_absoluto = s_matkl[ matkl = <f0041>-matkl ]-desc_absoluto.

*      IF WA_ZSDT0040-WAERK EQ 'USD'.
*        <F0041>-VLRTOT        = S_MATKL[ MATKL = <F0041>-MATKL ]-VLRTOT  * WA_ZSDT0117-KURSK.
*      ELSE.
      <f0041>-vlrtot        = s_matkl[ matkl = <f0041>-matkl ]-vlrtot.
*      ENDIF.

      <f0041>-vlr_frete     = s_matkl[ matkl = <f0041>-matkl ]-vlr_frete.

      <f0041>-vlr_ajuste = s_matkl[ matkl = <f0041>-matkl ]-vlr_ajuste.

    ENDLOOP.

    i_0041[] = it_0041[].

  ENDMETHOD.


  METHOD CALC_TOT.

    DATA: VL_TAXA TYPE KURSF.

    VL_TAXA = I_TAXA.

    IF VL_TAXA IS INITIAL.
      VL_TAXA = 1.
    ENDIF.

    CASE ME->AT_TIPO.
      WHEN 'VDI'.

        CASE ME->AT_MATKL.
          WHEN '658445' OR '700460'. " DEFENSIVOS " RAMON adicionado o valor 700460 - 26.04.2024.
            I_VALOR  = ( ME->AT_CADENCIA * ME->AT_NETPR ) * VL_TAXA.

          WHEN '700150' OR '658440'. " FERTILIZANTES
            CASE ME->AT_KMEIN.
              WHEN 'TO'.
                I_VALOR = ( ( ME->AT_CADENCIA * ME->AT_NETPR ) / 1000 ) * VL_TAXA.
              WHEN 'BAG' OR 'SAC'.
                I_VALOR = ( ( ME->AT_CADENCIA * ME->AT_NETPR ) / ME->AT_BRGEW ) * VL_TAXA.
              WHEN OTHERS.
                I_VALOR = ( ME->AT_CADENCIA * ME->AT_NETPR ) * VL_TAXA.
            ENDCASE.

          WHEN '700240'.
            CASE ME->AT_KMEIN.
              WHEN 'TO'.
                I_VALOR = ( ( ME->AT_CADENCIA * ME->AT_NETPR ) / ME->AT_BRGEW ) * VL_TAXA.
              WHEN 'BAG' OR 'SAC'.
                I_VALOR = ( ME->AT_CADENCIA * ME->AT_NETPR ) * VL_TAXA.
              WHEN OTHERS.
                I_VALOR = ( ME->AT_CADENCIA * ME->AT_NETPR * ME->AT_BRGEW ) * VL_TAXA.
            ENDCASE.

          WHEN '700230' OR '700130'. " SEMENTES
            CASE ME->AT_KMEIN.
              WHEN 'TO'.
                I_VALOR = ( ( ME->AT_CADENCIA * ME->AT_NETPR ) / 1000 ) * VL_TAXA.
              WHEN 'BAG' OR 'SAC'.
                I_VALOR = ( ( ME->AT_CADENCIA * ME->AT_NETPR ) / ME->AT_BRGEW ) * VL_TAXA.
              WHEN OTHERS.
                I_VALOR = ( ME->AT_CADENCIA * ME->AT_NETPR ) * VL_TAXA.
            ENDCASE.
        ENDCASE.

      WHEN 'FRI'.

        CASE ME->AT_MATKL.
          WHEN '658445' OR '700460'. " DEFENSIVOS " RAMON adicionado o valor 700460 - 26.04.2024.
            I_VALOR  = ME->AT_CADENCIA * ME->AT_FRETE.
          WHEN '700150' OR '658440'. " FERTILIZANTES
            CASE I_ZIEME.
              WHEN 'TO'.
                I_VALOR = ( ME->AT_CADENCIA * ME->AT_FRETE ) / 1000.
              WHEN 'BAG' OR 'SAC'.
                I_VALOR = ( ME->AT_CADENCIA * ME->AT_FRETE ) / ME->AT_BRGEW.
              WHEN OTHERS.
                I_VALOR = ME->AT_CADENCIA * ME->AT_FRETE.
            ENDCASE.

          WHEN '700240'.
            CASE I_ZIEME.
              WHEN 'TO'.
                I_VALOR =  ( ME->AT_CADENCIA * ME->AT_FRETE ) / ME->AT_BRGEW .
              WHEN 'BAG' OR 'SAC'.
                I_VALOR =  ( ME->AT_CADENCIA * ME->AT_FRETE ) .
              WHEN OTHERS.
                I_VALOR =  ( ME->AT_CADENCIA * ME->AT_FRETE ) * ME->AT_BRGEW .
            ENDCASE.

          WHEN '700230' OR '700130'. " SEMENTES
            CASE I_ZIEME.
              WHEN 'TO'.
                I_VALOR =  ( ME->AT_CADENCIA * ME->AT_FRETE ) / 1000.
              WHEN 'BAG' OR 'SAC'.
                I_VALOR =  ( ME->AT_CADENCIA * ME->AT_FRETE ) / ME->AT_BRGEW.
              WHEN OTHERS.
                I_VALOR =   ME->AT_CADENCIA * ME->AT_FRETE.
            ENDCASE.
        ENDCASE.
    ENDCASE.

    CASE ME->AT_MATKL.
      WHEN '658445' OR '700460'. " DEFENSIVOS " RAMON adicionado o valor 700460 - 26.04.2024.
        ME->SET_CADENCIA_IN( 0 ).
        ME->SET_ZIEME( '' ).
      WHEN '700150' OR '658440' OR '700230' OR '700130'.
        ME->SET_ZIEME( 'KG' ).
      WHEN '700240'.
        ME->SET_ZIEME( 'BAG' ).
    ENDCASE.

    ME->SET_TOTAL_PROPORCIONAL( I_VALOR ).

  ENDMETHOD.


  METHOD CHECK_AUART.

    DATA(OBJ_AUART) = NEW ZCL_TAXA_CURVA( ).

    DATA: R_AUART TYPE RANGE OF AUART.

    IF TIPO EQ 'VDA' OR TIPO EQ 'FRE'.

      SELECT SINGLE AUART
        FROM ZSDT0051
        INTO @DATA(_AUART)
          WHERE NRO_SOL_OV EQ @NUMERO.

    ELSE.
      SY-SUBRC = 4.
    ENDIF.

    RETURN = SY-SUBRC.
    CHECK SY-SUBRC IS INITIAL.

    R_AUART = SWITCH #( TIPO
                        WHEN 'FRE' THEN OBJ_AUART->GET_AUART( 'ZHEDGEFRE' )
                        WHEN 'VDA' THEN OBJ_AUART->GET_AUART( 'ZHEDGEVDA' )
                      ) .

    RETURN = COND #( WHEN _AUART IN R_AUART THEN 0 ELSE 4 ).

  ENDMETHOD.


  METHOD ESTORNO_VF11.

    DATA: GT_ZSDT0053     TYPE TABLE OF ZSDT0053,
          GW_ZSDT0053     TYPE ZSDT0053,
          GT_ZSDT0059     TYPE TABLE OF ZSDT0059,
          GW_ZSDT0059     TYPE ZSDT0059,
          GW_ZSDT0073     TYPE ZSDT0073,
          GT_ZSDT0100     TYPE TABLE OF ZSDT0100,
          GW_ZSDT0100     TYPE ZSDT0100,
          GT_ZSDT0100_AUX TYPE TABLE OF ZSDT0100,
          GW_ZSDT0100_AUX TYPE ZSDT0100,
          DIRECAO         TYPE C LENGTH 1,
          VAR_MONTANTE    TYPE ZSDT0053-ZMENG,
          LINHAS          TYPE SY-TABIX.

    DATA(OBJ_AUART) = NEW ZCL_TAXA_CURVA( ).

    DATA: R_COMP TYPE RANGE OF AUART.
    DATA: R_DEVO_RECU TYPE RANGE OF AUART.

    R_COMP = OBJ_AUART->GET_AUART( 'ZHEDGECOMP' ). " Get SET de AUART de Complemento
    R_DEVO_RECU = OBJ_AUART->GET_AUART( 'ZHEDGEDEVO/RECU' ). " Get SET de AUART de Devolução/Recusa

    IF I_AUART IN R_COMP.
      DIRECAO = 'W'.
    ELSEIF I_AUART IN R_DEVO_RECU.
      DIRECAO = 'Y'.
    ENDIF.

*    CASE I_AUART.
*      WHEN 'ZCPV' OR 'ZCOP' OR 'ZCFX'. DIRECAO = 'W'.
*      WHEN 'ZREB' OR 'ZROB' OR 'ZDMI'. DIRECAO = 'Y'.
*    ENDCASE.

    IF I_TCODE EQ 'VF11'.
      IF I_ZSDT0094-TIPO EQ 'VDA'.

        IF DIRECAO EQ 'W'.

          SELECT * FROM ZSDT0100
           INTO TABLE GT_ZSDT0100
             WHERE NRO_SOL_OV EQ I_ZSDT0094-NRO_SOL_OV
               AND FIXACAO EQ I_ZSDT0094-FIXACAO
               AND AUART IN R_COMP
               AND STATUS NE 'C'.

        ELSEIF DIRECAO EQ 'Y'.

          SELECT * FROM ZSDT0100
            INTO TABLE GT_ZSDT0100
              WHERE NRO_SOL_OV EQ I_ZSDT0094-NRO_SOL_OV
                AND FIXACAO EQ I_ZSDT0094-FIXACAO
                AND AUART IN R_DEVO_RECU
                AND STATUS NE 'C'.
        ENDIF.

        IF GT_ZSDT0100 IS INITIAL.

          SELECT SINGLE * FROM ZSDT0053
            INTO GW_ZSDT0053
              WHERE VBELN EQ I_VBELN.

          UPDATE ZSDT0053 SET STATUS = 'C'
                              VBELN  = ''
                              WHERE STATUS EQ DIRECAO
                                AND NRO_SOL_OV EQ I_ZSDT0094-NRO_SOL_OV
                                AND FIXACAO EQ I_ZSDT0094-FIXACAO
                                AND CHARG EQ I_ZSDT0094-SAFRA.

          IF GW_ZSDT0053 IS NOT INITIAL AND GW_ZSDT0053-FIXACAO IS NOT INITIAL.

            GW_ZSDT0053-ZMENG = GW_ZSDT0053-ZMENG - ( I_ZSDT0094-CADENCIA_QTE ).
            UPDATE ZSDT0059 SET FORMULA  = '0.0000'
                                FORMULA2 = '0.0000'
                                MONAT    = '00'
                                VALDT = '00000000'
                                POSNR1 = '000000'
                                VALDT_HEDGE = '00000000'
                                WHERE NRO_SOL_OV EQ GW_ZSDT0053-NRO_SOL_OV
                                  AND POSNR EQ GW_ZSDT0053-FIXACAO
                                  AND FIELD EQ 'QTDFIXADA'
                                  AND POSNR1 EQ GW_ZSDT0053-POSNR.

            UPDATE ZSDT0059 SET FORMULA  = GW_ZSDT0053-ZMENG
                                FORMULA2 = GW_ZSDT0053-ZMENG
                                VALDT = '00000000'
                                MONAT    = '00'
                                POSNR1 = '000000'
                                VALDT_HEDGE = '00000000'
                                WHERE NRO_SOL_OV EQ GW_ZSDT0053-NRO_SOL_OV
                                  AND POSNR EQ GW_ZSDT0053-FIXACAO
                                  AND FIELD EQ 'PRECO'
                                  AND POSNR1 EQ GW_ZSDT0053-POSNR.


            SELECT SINGLE * FROM ZSDT0073
              INTO GW_ZSDT0073
                WHERE NRO_SOL_OV = GW_ZSDT0053-NRO_SOL_OV
                  AND FIXACAO    = GW_ZSDT0053-FIXACAO.

            IF SY-SUBRC IS INITIAL.
              GW_ZSDT0073-QTE_VENC = GW_ZSDT0073-QTE_VENC - 1.
              UPDATE ZSDT0073 SET QTE_VENC = GW_ZSDT0073-QTE_VENC
                                  WHERE NRO_SOL_OV = GW_ZSDT0073-NRO_SOL_OV
                                    AND FIXACAO    = GW_ZSDT0073-FIXACAO.
            ENDIF.
          ENDIF.

        ELSE.

          CLEAR: GW_ZSDT0053, GW_ZSDT0100.
          REFRESH: GT_ZSDT0053, GT_ZSDT0100_AUX.

          SELECT * FROM ZSDT0100
            INTO TABLE GT_ZSDT0100_AUX
              WHERE VBELN EQ I_VBELN.

          SELECT * FROM ZSDT0053
            INTO TABLE GT_ZSDT0053
            FOR ALL ENTRIES IN GT_ZSDT0100_AUX
              WHERE VBELN EQ GT_ZSDT0100_AUX-VBELN.

          IF GT_ZSDT0053 IS NOT INITIAL.

            IF DIRECAO EQ 'W'.

              SELECT * FROM ZSDT0100
               INTO TABLE @DATA(GT_ZSDT0100_AUX2)
                FOR ALL ENTRIES IN @GT_ZSDT0100_AUX
                 WHERE NRO_SOL_OV EQ @GT_ZSDT0100_AUX-NRO_SOL_OV
                   AND FIXACAO EQ @GT_ZSDT0100_AUX-FIXACAO
                   AND ( POSNR EQ @GT_ZSDT0100_AUX-POSNR OR POSNR IS NULL )
                   AND AUART IN @R_COMP
                   AND STATUS NE 'C'.

            ELSEIF DIRECAO EQ 'Y'.

              SELECT * FROM ZSDT0100
               INTO TABLE GT_ZSDT0100_AUX2
                FOR ALL ENTRIES IN GT_ZSDT0100_AUX
                 WHERE NRO_SOL_OV EQ GT_ZSDT0100_AUX-NRO_SOL_OV
                   AND FIXACAO EQ GT_ZSDT0100_AUX-FIXACAO
                   AND ( POSNR EQ GT_ZSDT0100_AUX-POSNR OR POSNR IS NULL )
                   AND AUART IN R_DEVO_RECU
                   AND STATUS NE 'C'.

            ENDIF.

            CLEAR: GW_ZSDT0053, GW_ZSDT0100.
            SORT GT_ZSDT0100_AUX2 BY VBELN.
            LINHAS = LINES( GT_ZSDT0100_AUX2 ).
            READ TABLE GT_ZSDT0100_AUX2 INTO GW_ZSDT0100 INDEX LINHAS.

            UPDATE ZSDT0053 SET VBELN = GW_ZSDT0100-VBELN
                                WHERE STATUS EQ DIRECAO
                                  AND NRO_SOL_OV EQ I_ZSDT0094-NRO_SOL_OV
                                  AND FIXACAO EQ I_ZSDT0094-FIXACAO
                                  AND POSNR EQ GW_ZSDT0100-POSNR.

          ENDIF.

          SELECT SINGLE * FROM ZSDT0053
            INTO GW_ZSDT0053
              WHERE STATUS EQ DIRECAO
                AND NRO_SOL_OV EQ I_ZSDT0094-NRO_SOL_OV
                AND FIXACAO EQ I_ZSDT0094-FIXACAO
                AND CHARG EQ I_ZSDT0094-SAFRA.

          IF GW_ZSDT0053 IS NOT INITIAL.

            GW_ZSDT0053-ZMENG = GW_ZSDT0053-ZMENG - ( I_ZSDT0094-CADENCIA_QTE ).

            IF ( GW_ZSDT0053-ZIEME EQ  GW_ZSDT0053-PMEIN ).
              GW_ZSDT0053-VLRTOT = GW_ZSDT0053-ZMENG * GW_ZSDT0053-DMBTR.
            ELSE.
              IF ( GW_ZSDT0053-ZIEME EQ 'KG' ) AND ( GW_ZSDT0053-PMEIN EQ 'TO' ).
                VAR_MONTANTE = GW_ZSDT0053-ZMENG / 1000.
                GW_ZSDT0053-VLRTOT = VAR_MONTANTE * GW_ZSDT0053-DMBTR.
              ELSEIF ( GW_ZSDT0053-ZIEME EQ 'TO' ) AND ( GW_ZSDT0053-PMEIN EQ 'KG' ).
                VAR_MONTANTE = GW_ZSDT0053-ZMENG * 1000.
                GW_ZSDT0053-VLRTOT = VAR_MONTANTE * GW_ZSDT0053-DMBTR.
              ENDIF.
            ENDIF.

            UPDATE ZSDT0053 SET ZMENG = GW_ZSDT0053-ZMENG
                                VLRTOT = GW_ZSDT0053-VLRTOT
                                WHERE STATUS EQ DIRECAO
                                  AND NRO_SOL_OV EQ I_ZSDT0094-NRO_SOL_OV
                                  AND FIXACAO EQ I_ZSDT0094-FIXACAO
                                  AND CHARG EQ I_ZSDT0094-SAFRA.

            UPDATE ZSDT0059 SET FORMULA  = GW_ZSDT0053-ZMENG
                                FORMULA2 = GW_ZSDT0053-ZMENG
                                WHERE NRO_SOL_OV EQ GW_ZSDT0053-NRO_SOL_OV
                                  AND FIELD EQ 'QTDFIXADA'
                                  AND POSNR EQ GW_ZSDT0053-FIXACAO
                                  AND POSNR1 EQ GW_ZSDT0053-POSNR.
          ENDIF.
*        ENDIF.

        ENDIF.
      ENDIF.

    ENDIF.

    COMMIT WORK.
    WAIT UP TO 2 SECONDS.

  ENDMETHOD.


  METHOD GET_AUART.

    FREE: R_RANGE.

* Seleciona o SET informado
    IF SET NE 'TODOS'.

      SELECT *
        FROM SETLEAF
        INTO TABLE @DATA(IT_SET)
       WHERE SETNAME = @SET.

    ELSE.

      SELECT *
        FROM SETLEAF
        APPENDING TABLE IT_SET
       WHERE SETNAME = 'ZHEDGECOMP'.

      SELECT *
        FROM SETLEAF
        APPENDING TABLE IT_SET
       WHERE SETNAME = 'ZHEDGEDEVO/RECU'.

    ENDIF.

* Ragen de Sets
    LOOP AT IT_SET INTO DATA(WA_SET).
      APPEND VALUE RSDSSELOPT( OPTION = WA_SET-VALOPTION
                               SIGN   = WA_SET-VALSIGN
                               LOW    = WA_SET-VALFROM )
                          TO R_RANGE.
    ENDLOOP.


  ENDMETHOD.


  METHOD GET_BEZEI.
    E_BEZEI = ME->AT_BEZEI.
  ENDMETHOD.


  METHOD GET_CADENCIA.
    E_CADENCIA = ME->AT_CADENCIA.
  ENDMETHOD.


  METHOD GET_DATA_LIB.
    E_DATA_LIB = ME->AT_DATA_LIB.
  ENDMETHOD.


  METHOD GET_DATA_REGISTRO.
    E_DATA = ME->AT_DATA_REGISTRO.
  ENDMETHOD.


  METHOD GET_DATA_VENC.
    E_DATA_VENC = ME->AT_DATA_VENC.
  ENDMETHOD.


  METHOD GET_ESTORNO.
    E_RETORNO = ME->AT_ESTORNO.
  ENDMETHOD.


  METHOD GET_FIRTS_FRETE.

    DATA: CONT TYPE I.

    DATA(VBELN) = I_VBELN.
    DATA(MATNR) = I_MATNR.

    IF NOT VBELN IS INITIAL.
      CHECK ME->GET_TABLES( VBELN ) NE 8.
    ELSE.
      EXIT.
    ENDIF.

    SELECT SINGLE * FROM ZSDT0041
      INTO W_0041
      WHERE DOC_SIMULACAO EQ I_DOC_SIMULACAO
      AND VBELN EQ VBELN
      AND MATNR EQ MATNR.

    CLEAR CONT.
    WHILE NOT SY-SUBRC IS INITIAL.
      ADD 1 TO CONT.

      IF CONT > 500.
        SY-SUBRC = 0.
        EXIT.
      ENDIF.

      SELECT SINGLE * FROM ZSDT0090
        INTO @DATA(WA_0090)
        WHERE DOC_SIMULACAO EQ @I_DOC_SIMULACAO
          AND VBELN EQ @VBELN
          AND MATNR EQ @MATNR.

      CHECK SY-SUBRC IS INITIAL.

      VBELN = WA_0090-VBELV.
      MATNR = WA_0090-MATNRV.

      SELECT SINGLE * FROM ZSDT0041
        INTO W_0041
        WHERE DOC_SIMULACAO EQ I_DOC_SIMULACAO
          AND VBELN EQ VBELN
          AND MATNR EQ MATNR.

    ENDWHILE.

  ENDMETHOD.


  METHOD GET_FOBS.
    E_VALOR = ME->AT_FOBS.
  ENDMETHOD.


  METHOD GET_FRETE_CIF.
    CONDENSE ME->AT_FRETE_CIF NO-GAPS.
    E_VALOR = ME->AT_FRETE_CIF.
  ENDMETHOD.


  METHOD GET_FRETE_IN.
    E_FRETE = ME->AT_FRETE.
  ENDMETHOD.


  METHOD GET_FRETE_PORTO.

    CONDENSE ME->AT_FRETE_PORTO NO-GAPS.
    E_VALOR = ME->AT_FRETE_PORTO.

  ENDMETHOD.


  METHOD GET_HORA_REGISTRO.

    E_HORA = ME->AT_HORA_REGISTRO.
  ENDMETHOD.


  METHOD GET_INCOTERMS.
    E_INCO1 = ME->AT_INCO1.
  ENDMETHOD.


  method GET_INTERCOMPANY.
      E_ICPANY = ME->AT_ICPANY.
  endmethod.


  METHOD GET_MATKL.
    E_MATKL = ME->AT_MATKL.
  ENDMETHOD.


  METHOD GET_NETPR.
    E_NETPR = ME->AT_NETPR.
  ENDMETHOD.


  METHOD GET_NUMERO.
    E_NUMERO = ME->AT_NUMERO.
  ENDMETHOD.


  METHOD GET_POSNR.
    E_POSNR = ME->AT_POSNR.
  ENDMETHOD.


  METHOD GET_SAFRA.
    E_SAFRA = ME->AT_SAFRA.
  ENDMETHOD.


  METHOD GET_TABLES.

    SELECT COUNT(*) FROM ZSDT0041
      WHERE VBELN EQ I_VBELN.

    ADD SY-SUBRC TO AT_CHECK.

    SELECT COUNT(*) FROM ZSDT0090
      WHERE VBELN EQ I_VBELN.

    ADD SY-SUBRC TO AT_CHECK.

    RETURN = AT_CHECK.


  ENDMETHOD.


  METHOD GET_TAXA_CAMBIO.
    E_TAXA = ME->AT_TAXA_CAMBIO.
  ENDMETHOD.


  METHOD GET_TAXA_CURVA.
    E_TAXA = ME->AT_TAXA_CURVA.
  ENDMETHOD.


  METHOD GET_TAXA_IN.
    E_TAXA = ME->AT_TAXA.
  ENDMETHOD.


  METHOD GET_TIPO.
    E_TIPO = ME->AT_TIPO.
  ENDMETHOD.


  METHOD get_tipo_auart.

    DATA: vl_auart TYPE auart.

    vl_auart = COND #( WHEN auart IS NOT INITIAL THEN auart ELSE vbrk-fkart ).

    DATA(obj_tx_curva)    = NEW zcl_taxa_curva( ).
    DATA(r_aqv) = obj_tx_curva->get_auart( 'RV60AFZZ_HEDGE_AQV' ).
    DATA(r_spt) = obj_tx_curva->get_auart( 'RV60AFZZ_HEDGE_SPT' ).
    DATA(r_tbo) = obj_tx_curva->get_auart( 'RV60AFZZ_HEDGE_TB' ).

    return = COND #( WHEN vl_auart IN r_aqv THEN 'AQV' ELSE
             COND #( WHEN vl_auart IN r_spt THEN 'SPT' ELSE
             COND #( WHEN vl_auart IN r_tbo THEN
             COND #( WHEN vbrk-vkorg EQ '0001' AND vbrk-bupla EQ '0161'
                                              THEN 'TBP' " Transbordo PortoChuelo
                                              ELSE 'TBO' ) "Transbordo
                   ) ) ).

  ENDMETHOD.


  METHOD GET_TIPO_TAXA.
    E_TIPO = ME->AT_TIPO_TAXA.
  ENDMETHOD.


  METHOD GET_TOTAL_PROPORCIONAL.
    E_TOTAL = ME->AT_TOTAL_PROPORCIONAL.
  ENDMETHOD.


  METHOD GET_VBELN.
    E_VBELN = ME->AT_VBELN.
  ENDMETHOD.


  METHOD GET_ZIEME.
    E_ZIEME = ME->AT_ZIEME.
  ENDMETHOD.


  METHOD SET_BEZEI.
    ME->AT_BEZEI = I_BEZEI.
  ENDMETHOD.


  METHOD SET_CADENCIA.

    DATA: VAR_VLR_PORTO TYPE DMBTR.

    CASE I_NEGATIVA.
      WHEN: 'X'.
        ME->AT_CADENCIA = I_CADENCIA * -1.
      WHEN OTHERS.
        CASE I_TCODE.
          WHEN: 'VF01'.

            VAR_VLR_PORTO = ME->GET_FRETE_PORTO( ).
            IF ( ME->GET_FRETE_CIF( ) > VAR_VLR_PORTO ).
              IF ( ME->GET_TIPO( ) EQ 'FRE' ) AND ( GET_TIPO_TAXA( ) EQ 'C' ) AND ( I_CADENCIA < 0 ).
                ME->AT_CADENCIA = I_CADENCIA * -1.
              ELSEIF ( ME->GET_TIPO( ) EQ 'FRE' ) AND ( GET_TIPO_TAXA( ) EQ 'C').
                ME->AT_CADENCIA = I_CADENCIA.
              ELSEIF ( ME->GET_TIPO( ) EQ 'FRE' ) AND ( GET_TIPO_TAXA( ) EQ 'V').
                ME->AT_CADENCIA = I_CADENCIA * -1.
              ELSE.
                ME->AT_CADENCIA = I_CADENCIA.
              ENDIF.
            ELSE.
*            IF ( ME->GET_FRETE_CIF( ) > VAR_VLR_PORTO ).
              IF ( ME->GET_TIPO( ) EQ 'FRE' ) AND ( GET_TIPO_TAXA( ) EQ 'C' ) AND ( I_CADENCIA < 0 ).
                ME->AT_CADENCIA = I_CADENCIA * -1.
              ELSEIF ( ME->GET_TIPO( ) EQ 'FRE' ) AND ( GET_TIPO_TAXA( ) EQ 'C').
                ME->AT_CADENCIA = I_CADENCIA.
              ELSE.
                ME->AT_CADENCIA = I_CADENCIA.
              ENDIF.
*            ENDIF.
            ENDIF.

          WHEN: 'ZSDT0044'.
            ME->AT_CADENCIA = I_CADENCIA.

          WHEN OTHERS.
            IF ( GET_TIPO( ) EQ 'FRE' ) AND ( GET_TIPO_TAXA( ) EQ 'V' ).
              ME->AT_CADENCIA = I_CADENCIA * -1.
            ELSE.
              ME->AT_CADENCIA = I_CADENCIA.
            ENDIF.

        ENDCASE.
    ENDCASE.
  ENDMETHOD.


  METHOD set_cadencia_in.

    IF i_estorno IS INITIAL.
      CASE me->at_matkl.
        WHEN '658445' OR '700460'. " DEFENSIVOS

          CASE i_negativa.
            WHEN 'S'.
              me->at_cadencia = i_cadencia * -1.
              IF NOT i_0040 IS INITIAL.
                me->set_data_venc( i_0040-dt_entrega_def ).
              ENDIF.
            WHEN 'N'.
              me->at_cadencia = i_cadencia.
            WHEN OTHERS.
              me->at_cadencia = i_cadencia.
          ENDCASE.

          me->set_zieme( '' ).

          IF me->get_bezei( ) NS 'DF'.
            me->set_bezei( |DF{ me->get_bezei( ) }| ).
          ENDIF.

        WHEN '700150' OR '658440'. " FERTILIZANTES

          CASE i_negativa.
            WHEN 'S'.
              me->at_cadencia = i_cadencia * -1.
              IF NOT i_0040 IS INITIAL.
                me->set_data_venc( i_0040-dt_entrega_fet ).
              ENDIF.
            WHEN 'N'.
              me->at_cadencia = i_cadencia.
            WHEN OTHERS.

              CASE me->at_zieme.
                WHEN 'TO'.
                  me->at_cadencia = i_cadencia * 1000.
                WHEN 'BAG'.
                  me->at_cadencia = i_cadencia * me->at_brgew.
                WHEN OTHERS.
                  me->at_cadencia = i_cadencia.
              ENDCASE.

              IF sy-cprog EQ 'ZSDR016'.
                CASE me->at_kmein.
                  WHEN 'TO'.
                    me->set_netpr( i_netpr = me->at_netpr / 1000 ).
                  WHEN 'BAG'.
                    me->set_netpr( i_netpr = me->at_netpr / me->at_brgew ).
                  WHEN OTHERS.
                    me->set_netpr( i_netpr = me->at_netpr ).
                ENDCASE.
              ENDIF.

          ENDCASE.

          me->set_zieme( 'KG' ).

          IF me->get_bezei( ) NS 'FT'.
            me->set_bezei( |FT{ me->get_bezei( ) }| ).
          ENDIF.

        WHEN '700230' OR '700130' OR '700240' OR '700350'. " SEMENTES

          CASE i_negativa.
            WHEN 'S'.
              me->at_cadencia = i_cadencia * -1.
              IF NOT i_0040 IS INITIAL.
                me->set_data_venc( i_0040-dt_entrega_sem ).
              ENDIF.
            WHEN 'N'.
              me->at_cadencia = i_cadencia.
            WHEN OTHERS.

              IF me->at_matkl NE '700240'.

                CASE me->at_zieme.
                  WHEN 'TO'.
                    me->at_cadencia = i_cadencia * 1000.
                  WHEN 'BAG'.
                    me->at_cadencia = i_cadencia * me->at_brgew.
                  WHEN OTHERS.
                    me->at_cadencia = i_cadencia.
                ENDCASE.

                IF sy-cprog EQ 'ZSDR016'.
                  CASE me->at_kmein.
                    WHEN 'TO'.
                      me->set_netpr( i_netpr = me->at_netpr / 1000 ).
                    WHEN 'BAG'.
                      me->set_netpr( i_netpr = me->at_netpr / me->at_brgew ).
                    WHEN OTHERS.
                      me->set_netpr( i_netpr = me->at_netpr ).
                  ENDCASE.
                ENDIF.

              ELSE.
                TRY .
                    CASE me->at_zieme.
                      WHEN 'TO'.
                        me->at_cadencia = ( i_cadencia * 1000 ) / me->at_brgew.
                      WHEN 'BAG'.
                        me->at_cadencia = i_cadencia.
                      WHEN OTHERS.
*==========================Adiciondo condição para tratar o DUMP em PRD / Validar se tem informações para fazer o calculo.
                        IF i_cadencia > 0 AND me->at_brgew > 0.
                          me->at_cadencia = i_cadencia / me->at_brgew.
*==========================Adiciondo condição para tratar o DUMP em PRD / Validar se tem informações para fazer o calculo.
                        ENDIF.
                    ENDCASE.
                  CATCH cx_sy_zerodivide.
                ENDTRY.

                IF sy-cprog EQ 'ZSDR016'.
                  CASE me->at_kmein.
                    WHEN 'TO'.
                      me->set_netpr( i_netpr = ( me->at_netpr / 1000 ) * me->at_brgew ).
                    WHEN 'BAG'.
                      me->set_netpr( i_netpr = me->at_netpr ).
                    WHEN OTHERS.
                      me->set_netpr( i_netpr = me->at_netpr * me->at_brgew ).
                  ENDCASE.
                ENDIF.

              ENDIF.

          ENDCASE.

          IF me->at_matkl NE '700240'.

            me->set_zieme( 'KG' ).

            IF me->at_matkl EQ '700350'.
              IF me->get_bezei( ) NS 'SA'.
                me->set_bezei( |SA{ me->get_bezei( ) }| ).
              ENDIF.
            ELSE.
              IF me->get_bezei( ) NS 'SS'.
                me->set_bezei( |SS{ me->get_bezei( ) }| ).
              ENDIF.
            ENDIF.

          ELSE.

            me->set_zieme( 'BAG' ).
            IF me->get_bezei( ) NS 'SM'.
              me->set_bezei( |SM{ me->get_bezei( ) }| ).
            ENDIF.

          ENDIF.

      ENDCASE.
    ELSE.
      me->at_cadencia = i_cadencia * -1.
    ENDIF.

  ENDMETHOD.


  METHOD set_cad_in_dentrega.
    CASE me->at_matkl.
      WHEN '658445' OR '700460'. " DEFENSIVOS " RAMON adicionado o valor 700460 - 26.04.2024.

        me->at_cadencia = i_cadencia.
        me->set_zieme( '' ).

        IF me->get_bezei( ) NS 'DF'.
          me->set_bezei( |DF{ me->get_bezei( ) }| ).
        ENDIF.

      WHEN '700150' OR '658440'. " FERTILIZANTES
        CASE me->at_zieme.
          WHEN 'TO'.
            me->at_cadencia = i_cadencia * 1000.
          WHEN 'BAG'.
            me->at_cadencia = i_cadencia * me->at_brgew.
          WHEN OTHERS.
            me->at_cadencia = i_cadencia.
        ENDCASE.

        me->set_zieme( 'KG' ).

        IF me->get_bezei( ) NS 'FT'.
          me->set_bezei( |FT{ me->get_bezei( ) }| ).
        ENDIF.

      WHEN '700230' OR '700130' OR '700240' OR '700350'. " SEMENTES

        IF me->at_matkl NE '700240'.
          CASE me->at_zieme.
            WHEN 'TO'.
              me->at_cadencia = i_cadencia * 1000.
            WHEN 'BAG'.
              me->at_cadencia = i_cadencia * me->at_brgew.
            WHEN OTHERS.
              me->at_cadencia = i_cadencia.
          ENDCASE.
        ELSE.
          CASE me->at_zieme.
            WHEN 'TO'.
              me->at_cadencia = ( i_cadencia * 1000 ) / me->at_brgew.
            WHEN 'BAG'.
              me->at_cadencia = i_cadencia.
            WHEN OTHERS.
              me->at_cadencia = i_cadencia / me->at_brgew.
          ENDCASE.
        ENDIF.

        IF me->at_matkl NE '700240'.
          me->set_zieme( 'KG' ).

          IF me->at_matkl EQ '700350'.
            IF me->get_bezei( ) NS 'SA'.
              me->set_bezei( |SA{ me->get_bezei( ) }| ).
            ENDIF.
          ELSE.
            IF me->get_bezei( ) NS 'SS'.
              me->set_bezei( |SS{ me->get_bezei( ) }| ).
            ENDIF.
          ENDIF.
        ELSE.
          me->set_zieme( 'BAG' ).
          IF me->get_bezei( ) NS 'SM'.
            me->set_bezei( |SM{ me->get_bezei( ) }| ).
          ENDIF.

        ENDIF.

    ENDCASE.
  ENDMETHOD.


  METHOD SET_DATA_LIB.
    IF ( SY-CPROG NE 'ZCARGA').
      ME->AT_DATA_LIB = SY-DATUM.
    ELSE.
      ME->AT_DATA_LIB = I_DATA_LIB.
    ENDIF.

  ENDMETHOD.


  METHOD SET_DATA_REGISTRO.
    ME->AT_DATA_REGISTRO = I_DATA.
  ENDMETHOD.


  METHOD SET_DATA_VENC.

    DATA S_SUBRC TYPE SY-SUBRC.

    IF ( I_DATA_VENC < SY-DATUM ).
      ME->AT_DATA_VENC = SY-DATUM + 30.
    ELSE.
      ME->AT_DATA_VENC = I_DATA_VENC.
    ENDIF.

    DO.
      IF S_SUBRC IS NOT INITIAL.
        EXIT.
      ENDIF.
      ZCL_SOLICITACAO_OV=>DIA_UTIL( EXPORTING P_VENCIMENTO = ME->AT_DATA_VENC
                                     IMPORTING E_SUBRC      = S_SUBRC
                                   ).
      IF S_SUBRC IS INITIAL.
        ADD 1 TO ME->AT_DATA_VENC.
      ENDIF.

    ENDDO.

    E_DATA = ME->AT_DATA_VENC.

  ENDMETHOD.


  METHOD SET_ESTORNO.
    ME->AT_ESTORNO = I_NUMERO.
  ENDMETHOD.


  METHOD SET_FOBS.
    ME->AT_FOBS = I_VALOR.
  ENDMETHOD.


  METHOD SET_FRETE_CIF.
    ME->AT_FRETE_CIF = I_VALOR.
  ENDMETHOD.


  METHOD SET_FRETE_IN.

    IF I_ZIEME IS INITIAL.

      CASE ME->AT_MATKL.
        WHEN '658445'  OR '700460'. " DEFENSIVOS " RAMON adicionado o valor 700460 - 26.04.2024.
          ME->SET_TOTAL_PROPORCIONAL( ME->AT_CADENCIA * I_FRETE ).
          ME->SET_CADENCIA_IN( 0 ).

        WHEN '700150' OR '658440'. " FERTILIZANTES
          CASE ME->AT_ZIEME.
            WHEN 'TO'.
              ME->SET_TOTAL_PROPORCIONAL( ( ME->AT_CADENCIA * I_FRETE ) / 1000 ).
            WHEN 'BAG' OR 'SAC'.
              ME->SET_TOTAL_PROPORCIONAL( ( ME->AT_CADENCIA * I_FRETE ) / ME->AT_BRGEW ).
            WHEN OTHERS.
              ME->SET_TOTAL_PROPORCIONAL( ME->AT_CADENCIA * I_FRETE ).
          ENDCASE.

        WHEN '700230' OR '700130' OR '700350'. " SEMENTES
          CASE ME->AT_ZIEME.
            WHEN 'TO'.
              ME->SET_TOTAL_PROPORCIONAL( ( ME->AT_CADENCIA * I_FRETE ) / 1000 ).
            WHEN 'BAG' OR 'SAC'.
              ME->SET_TOTAL_PROPORCIONAL( ( ME->AT_CADENCIA * I_FRETE ) / ME->AT_BRGEW ).
            WHEN OTHERS.
              ME->SET_TOTAL_PROPORCIONAL( ME->AT_CADENCIA * I_FRETE ).
          ENDCASE.

        WHEN '700240'. " SEMENTES
          CASE ME->AT_ZIEME.
            WHEN 'TO'.
              ME->SET_TOTAL_PROPORCIONAL( ( ME->AT_CADENCIA * I_FRETE ) / ME->AT_BRGEW ).
            WHEN 'BAG' OR 'SAC'.
              ME->SET_TOTAL_PROPORCIONAL( ( ME->AT_CADENCIA * I_FRETE ) ).
            WHEN OTHERS.
              ME->SET_TOTAL_PROPORCIONAL( ( ME->AT_CADENCIA * I_FRETE ) * ME->AT_BRGEW ).
          ENDCASE.

      ENDCASE.

    ELSE.

      IF  I_ZIEME NE ME->AT_ZIEME. "I_ZIEME Unidade de Medida do Frete || ME->AT_ZIEME Unidade de Medida da Quantidade
        CASE ME->AT_ZIEME. " Unidade de Medida da Quantidade

          WHEN 'KG'.
            CASE I_ZIEME. "Unidade de Medida do Frete
              WHEN 'TO'.
                ME->SET_TOTAL_PROPORCIONAL( ( ME->AT_CADENCIA / 1000 ) * I_FRETE ).
              WHEN 'BAG' OR 'SAC'.
                ME->SET_TOTAL_PROPORCIONAL( ( ME->AT_CADENCIA / ME->AT_BRGEW ) * I_FRETE ).
              WHEN OTHERS.
                ME->SET_TOTAL_PROPORCIONAL( ME->AT_CADENCIA * I_FRETE ).
            ENDCASE.

          WHEN 'TO'.
            CASE I_ZIEME. "Unidade de Medida do Frete
              WHEN 'KG'.
                ME->SET_TOTAL_PROPORCIONAL( ( ME->AT_CADENCIA * 1000 ) * I_FRETE ).
              WHEN 'BAG' OR 'SAC'.
                ME->SET_TOTAL_PROPORCIONAL( ( ( ME->AT_CADENCIA * 1000 ) / ME->AT_BRGEW ) * I_FRETE ).
              WHEN OTHERS.
                ME->SET_TOTAL_PROPORCIONAL( ME->AT_CADENCIA * I_FRETE ).
            ENDCASE.

          WHEN 'BAG' OR 'SAC'.
            CASE I_ZIEME. "Unidade de Medida do Frete
              WHEN 'KG'.
                ME->SET_TOTAL_PROPORCIONAL( ( ME->AT_CADENCIA * ME->AT_BRGEW ) * I_FRETE ).
              WHEN 'TO'.
                ME->SET_TOTAL_PROPORCIONAL( ( ( ME->AT_CADENCIA * ME->AT_BRGEW ) / 1000 ) * I_FRETE ).
              WHEN OTHERS.
                ME->SET_TOTAL_PROPORCIONAL( ME->AT_CADENCIA * I_FRETE ).
            ENDCASE.

          WHEN OTHERS.
            ME->SET_TOTAL_PROPORCIONAL( ME->AT_CADENCIA * I_FRETE ).
        ENDCASE.

      ELSE.

        ME->SET_TOTAL_PROPORCIONAL( ( ME->AT_CADENCIA * I_FRETE ) ).
      ENDIF.
    ENDIF.

    CASE ME->AT_MATKL.
      WHEN '658445' OR '700460'. " DEFENSIVOS " RAMON adicionado o valor 700460 - 26.04.2024.
        ME->SET_CADENCIA_IN( 0 ).
        ME->SET_ZIEME( '' ).
      WHEN '700150' OR '658440' OR '700230' OR '700130' OR '700350'.
        ME->SET_ZIEME( 'KG' ).
      WHEN '700240'.
        ME->SET_ZIEME( 'BAG' ).
    ENDCASE.

    ME->AT_FRETE = I_FRETE.

  ENDMETHOD.


  METHOD SET_FRETE_PORTO.
    ME->AT_FRETE_PORTO = I_VALOR - ME->GET_FOBS( ).
  ENDMETHOD.


  METHOD SET_HORA_REGISTRO.

    ME->AT_HORA_REGISTRO = I_HORA.

  ENDMETHOD.


  METHOD SET_INCOTERMS.
    ME->AT_INCO1 = I_INCO1.
  ENDMETHOD.


  method SET_INTERCOMPANY.
    AT_ICPANY = S_ICPANY.
  endmethod.


  METHOD SET_MATKL.
    ME->AT_MATKL = I_MATKL.
    ME->AT_BRGEW = I_BRGEW.
  ENDMETHOD.


  METHOD SET_NETPR.
    ME->AT_NETPR = I_NETPR.
    ME->AT_KMEIN = I_KMEIN.
  ENDMETHOD.


  METHOD SET_NUMERO.
    ME->AT_NUMERO = I_NUMERO.

    CASE SY-CPROG.
      WHEN 'ZSDR016' OR 'ZSDR0042' OR 'SAPMV60A'.

        IF SY-CPROG EQ 'SAPMV60A'.
          CHECK I_TIPO = 'IN'. "Insumos
        ENDIF.

        IF I_NUMERO IS NOT INITIAL.
          SELECT SINGLE *
            FROM ZSDT0040 INTO @DATA(GW_ZSDT0040)
           WHERE DOC_SIMULACAO EQ @I_NUMERO.

          IF ( SY-SUBRC = 0 ) AND ( GW_ZSDT0040-SAFRA IS NOT INITIAL ).
            ME->SET_SAFRA(  GW_ZSDT0040-SAFRA ).
          ENDIF.
        ENDIF.
    ENDCASE.

  ENDMETHOD.


  METHOD SET_POSNR.
    ME->AT_POSNR = I_POSNR.
  ENDMETHOD.


  METHOD SET_SAFRA.
    ME->AT_SAFRA = I_SAFRA.
  ENDMETHOD.


  METHOD SET_TABLES.
    AT_CHECK = INPUT.
  ENDMETHOD.


  METHOD SET_TAXA_CAMBIO.

    DATA: IT_0094 TYPE TABLE OF ZSDT0094,
          WA_0094 TYPE ZSDT0094.

    SELECT * FROM ZSDT0094
      INTO TABLE IT_0094
      WHERE NRO_SOL_OV EQ I_NUMERO
      AND   DATA_VENC EQ I_DATA
      AND   TIPO EQ I_TIPO.

    SORT IT_0094 BY DATA_REGISTRO HORA_REGISTRO DESCENDING.

    IF ( I_TAXA IS INITIAL ).
      READ TABLE IT_0094 INTO WA_0094 INDEX 1.
      IF SY-SUBRC IS INITIAL.
        ME->AT_TAXA_CAMBIO = WA_0094-TAXA_CAMBIO.
      ELSE.
        ME->AT_TAXA_CAMBIO = ME->GET_TAXA_CURVA( ).
      ENDIF.
    ELSE.
      ME->AT_TAXA_CAMBIO = I_TAXA.
    ENDIF.

    IF I_ZERAR IS NOT INITIAL.
      ME->AT_TAXA_CAMBIO = 0.
    ENDIF.

  ENDMETHOD.


  METHOD SET_TAXA_CURVA.
    ME->AT_TAXA_CURVA = I_TAXA.
  ENDMETHOD.


  METHOD SET_TAXA_IN.

    SELECT SINGLE TAXA_CAMBIO
      FROM ZSDT0094
      INTO ME->AT_TAXA
      WHERE NRO_SOL_OV EQ ME->AT_NUMERO
        AND PROGRAMA EQ SY-CPROG
        AND DATA_VENC EQ ME->AT_DATA_VENC
        AND BEZEI EQ ME->AT_BEZEI
        AND TIPO EQ 'VDI'.

  ENDMETHOD.


  METHOD SET_TIPO.
    ME->AT_TIPO = I_TIPO.
  ENDMETHOD.


  METHOD SET_TIPO_TAXA.
    ME->AT_TIPO_TAXA = I_TIPO.
  ENDMETHOD.


  METHOD SET_TOTAL_PROPORCIONAL.
    CASE I_NEGATIVA.
      WHEN: 'X'.
        ME->AT_TOTAL_PROPORCIONAL = I_TOTAL * -1.
      WHEN OTHERS.
        ME->AT_TOTAL_PROPORCIONAL = I_TOTAL.
    ENDCASE.
  ENDMETHOD.


  METHOD SET_TX_CAMBIO_AQV.

    DATA: IT_0094 TYPE TABLE OF ZSDT0094,
          WA_0094 TYPE ZSDT0094.

    SELECT *
     FROM ZSDT0094
      INTO TABLE IT_0094
       WHERE   VBELN EQ I_VBELN
       AND DATA_VENC EQ I_DATA
       AND      TIPO EQ I_TIPO.

    SORT IT_0094 BY DATA_REGISTRO HORA_REGISTRO DESCENDING.

    IF ( I_TAXA IS INITIAL ).
      READ TABLE IT_0094 INTO WA_0094 INDEX 1.
      IF SY-SUBRC IS INITIAL.
        ME->AT_TAXA_CAMBIO = WA_0094-TAXA_CAMBIO.
      ELSE.
        ME->AT_TAXA_CAMBIO = ME->GET_TAXA_CURVA( ).
      ENDIF.
    ELSE.
      ME->AT_TAXA_CAMBIO = I_TAXA.
    ENDIF.

    IF I_ZERAR IS NOT INITIAL.
      ME->AT_TAXA_CAMBIO = 0.
    ENDIF.

  ENDMETHOD.


  METHOD SET_TX_CAMBIO_IN.

    DATA: IT_0094 TYPE TABLE OF ZSDT0094,
          WA_0094 TYPE ZSDT0094.

    SELECT *
     FROM ZSDT0094
      INTO TABLE IT_0094
       WHERE NRO_SOL_OV EQ I_NUMERO
         AND TIPO EQ I_TIPO
         AND ESTORNO EQ ABAP_FALSE.

    SORT IT_0094 BY DATA_REGISTRO HORA_REGISTRO ASCENDING.

    IF ( I_TAXA IS INITIAL ).
      READ TABLE IT_0094 INTO WA_0094 INDEX 1.
      IF SY-SUBRC IS INITIAL.
        ME->AT_TAXA_CAMBIO = WA_0094-TAXA_CAMBIO.
      ELSE.
        ME->AT_TAXA_CAMBIO = ME->GET_TAXA_CURVA( ).
      ENDIF.
    ELSE.
      ME->AT_TAXA_CAMBIO = I_TAXA.
    ENDIF.

    IF I_ZERAR IS NOT INITIAL.
      ME->AT_TAXA_CAMBIO = 0.
    ENDIF.

  ENDMETHOD.


  METHOD SET_VBELN.
    ME->AT_VBELN = I_VBELN.
  ENDMETHOD.


  METHOD SET_ZIEME.
    ME->AT_ZIEME = I_ZIEME.
  ENDMETHOD.


  METHOD TIPO_TAXA_IN.
    CONSTANTS: C_C TYPE C VALUE 'C',
               C_V TYPE C VALUE 'V'.

    CASE I_TIPO.
      WHEN 'VDI'. ME->SET_TIPO_TAXA( C_C ).
      WHEN 'FRI'. ME->SET_TIPO_TAXA( C_V ).
      WHEN 'EST'.
        CASE ME->AT_TIPO_TAXA.
          WHEN C_C. ME->SET_TIPO_TAXA( C_V ).
          WHEN C_V. ME->SET_TIPO_TAXA( C_C ).
        ENDCASE.
      WHEN 'AQT'.
        IF ME->AT_CADENCIA GE 0. ">=
          ME->SET_TIPO_TAXA( C_C ).
        ELSE.
          ME->SET_TIPO_TAXA( C_V ).
        ENDIF.
      WHEN OTHERS.
        IF ME->AT_TOTAL_PROPORCIONAL GT 0. ">
          ME->SET_TIPO_TAXA( C_C ).
        ELSE.
          ME->SET_TIPO_TAXA( C_V ).
        ENDIF.
    ENDCASE.

  ENDMETHOD.


  METHOD VERIFICA_TIPO_TAXA.

    CONSTANTS: C_C TYPE C VALUE 'C',
               C_V TYPE C VALUE 'V'.
    DATA: VAR_VLR_PORTO TYPE DMBTR.

    CASE I_TCODE.
      WHEN: 'ZSDT0062' OR 'ZSDT0066'.
        CASE ME->GET_TIPO( ).
          WHEN: 'VDA'.
            CASE ME->GET_BEZEI( ).
              WHEN: 'DIFERENCA'.
                IF ( ME->GET_TOTAL_PROPORCIONAL( ) < 0 ).
                  ME->SET_TIPO_TAXA( C_V ).
                ELSE.
                  ME->SET_TIPO_TAXA( C_C ).
                ENDIF.
              WHEN OTHERS.
                IF ( ME->GET_CADENCIA( ) < 0 ).
                  ME->SET_TIPO_TAXA( C_V ).
                ELSE.
                  ME->SET_TIPO_TAXA( C_C ).
                ENDIF.
            ENDCASE.
          WHEN: 'FRE'.
            IF ( I_TIPO IS INITIAL ).
              VAR_VLR_PORTO = ME->GET_FRETE_PORTO( ).
              IF ( ME->GET_FRETE_CIF( ) > VAR_VLR_PORTO ).
                ME->SET_TIPO_TAXA( C_V ).
              ELSE.
                ME->SET_TIPO_TAXA( C_C ).
              ENDIF.
            ELSE.
              IF ( ME->GET_CADENCIA( ) < 0 ).
                ME->SET_TIPO_TAXA( C_V ).
              ELSE.
                ME->SET_TIPO_TAXA( C_C ).
              ENDIF.
            ENDIF.
        ENDCASE.
      WHEN: 'VF01'.
        VAR_VLR_PORTO = ME->GET_FRETE_PORTO( ).
        CASE ME->GET_TIPO( ).
          WHEN: 'VDA'.
            IF ( ME->GET_CADENCIA( ) < 0 ).
              ME->SET_TIPO_TAXA( C_V ).
            ELSE.
              ME->SET_TIPO_TAXA( C_C ).
            ENDIF.
          WHEN: 'FRE'.
            IF ( I_TIPO NE 'EDI' ).
              IF I_STATUS EQ 'Y'.

                IF ( ME->GET_FRETE_CIF( ) > VAR_VLR_PORTO ).
                  ME->SET_TIPO_TAXA( C_C ).
                ELSE.
                  ME->SET_TIPO_TAXA( C_V ).
                ENDIF.

              ELSE.

                IF ( ME->GET_FRETE_CIF( ) > VAR_VLR_PORTO ).
                  ME->SET_TIPO_TAXA( C_V ).
                ELSE.
                  ME->SET_TIPO_TAXA( C_C ).
                ENDIF.

              ENDIF.
            ELSE.
              IF ( ME->GET_CADENCIA( ) < 0 ).
                ME->SET_TIPO_TAXA( C_V ).
              ELSE.
                ME->SET_TIPO_TAXA( C_C ).
              ENDIF.
            ENDIF.
        ENDCASE.
      WHEN: 'VF11'.
        VAR_VLR_PORTO = ME->GET_FRETE_PORTO( ).

        CASE ME->GET_TIPO( ).
          WHEN: 'VDA'.
            IF ( ME->GET_CADENCIA( ) < 0 ).
              ME->SET_TIPO_TAXA( C_V ).
            ELSE.
              ME->SET_TIPO_TAXA( C_C ).
            ENDIF.
          WHEN: 'FRE'.
            IF ( I_TIPO NE 'EDI' ).
              IF I_STATUS EQ 'Y'.

                IF ( ME->GET_FRETE_CIF( ) > VAR_VLR_PORTO ).
                  ME->SET_TIPO_TAXA( C_V ).
                ELSE.
                  ME->SET_TIPO_TAXA( C_C ).
                ENDIF.

              ELSE.

                IF ( ME->GET_FRETE_CIF( ) > VAR_VLR_PORTO ).
                  ME->SET_TIPO_TAXA( C_C ).
                ELSE.
                  ME->SET_TIPO_TAXA( C_V ).
                ENDIF.

              ENDIF.
            ELSE.
              IF ( ME->GET_CADENCIA( ) < 0 ).
                ME->SET_TIPO_TAXA( C_V ).
              ELSE.
                ME->SET_TIPO_TAXA( C_C ).
              ENDIF.
            ENDIF.
        ENDCASE.

*      CASE ME->GET_TIPO( ).
*        WHEN: 'VDA'.
*          ME->SET_TIPO_TAXA( C_C ).
*        WHEN: 'FRE'.
*          IF ( ME->GET_FRETE_CIF( ) > VAR_VLR_PORTO ).
*            ME->SET_TIPO_TAXA( C_V ).
*          ELSE.
*            ME->SET_TIPO_TAXA( C_C ).
*          ENDIF.
*      ENDCASE.
    ENDCASE.
  ENDMETHOD.


  METHOD set_taxa_cambio_boleta.

*    CHECK sy-cprog = 'ZSDR016'.
*
*    " 12.04.2024 - 96174 - RAMON -->
*    " BOLETAS
*    SELECT SINGLE * FROM zsdt0308
*      INTO @DATA(ls_308)
*        WHERE doc_simu = @i_numero
*          AND tp_obri = '8'.
*    " 12.04.2024 - 96174 - RAMON --<
*
*    CHECK ls_308-taxa_neg IS NOT INITIAL.
*
*    me->at_taxa_cambio = ls_308-taxa_neg.

" foi comentado pq deu um erro em PRD na ZSDT0062 no dia 13.11.2024 - request: DEVK9A276C e DEVK9A1Y32

  ENDMETHOD.
ENDCLASS.

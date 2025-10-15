FUNCTION zmmf_paral_busca_imposto.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(IE_PARAM) TYPE  ZMME_PARAL_BUSCA_IMPOSTO_PARAM
*"  EXPORTING
*"     VALUE(EE_PARAM) TYPE  ZMME_PARAL_BUSCA_IMPOSTO_PARAM
*"----------------------------------------------------------------------
*&----------------------------------------------------------------------*
*& Nome Objeto    : ZMMF_PARAL_BUSCA_IMPOSTO                            *
*& Chamado        : USER STORY 160696                                   *
*& Data           : 22/04/2025                                          *
*& Especificado   : Welgem Barbosa                                      *
*& Desenvolvimento: Nilton Marcelo Segantin                             *
*-----------------------------------------------------------------------*
*& Histórico de Alterações:                                             *
*-----------------------------------------------------------------------*
*&  Data     |Request    | Autor         | Alteração                    *
*&----------------------------------------------------------------------*
*&----------------------------------------------------------------------*
*& 22/04/2025|DEVK9A2EMI |NSEGATIN       | Melhoria de performance de   *
*&                                       |processamento de dados.       *
*&                                       |Chamado: 160696.              *
*-----------------------------------------------------------------------*

  TABLES: taxcom.

  TYPES: ty_konv TYPE TABLE OF komv.

  DATA: BEGIN OF t_konv OCCURS 0.
          INCLUDE STRUCTURE prcd_elements.
  DATA: END OF t_konv.

  DATA: es_ite  LIKE mepoitem.

  DATA: lv_vlr_unit_brt TYPE kaqty.

  FIELD-SYMBOLS: <lfa1>  TYPE lfa1,
                 <ekpo>  TYPE ekpo,
                 <ekko>  TYPE ekko,
                 <vorga> TYPE any,
                 <konv>  TYPE ty_konv,
                 <cva>   TYPE any,
                 <wmwst> TYPE any.

  CALL FUNCTION 'MEPO_DOC_ITEM_GET'
    EXPORTING
      im_ebelp = ie_param-ebelp
    IMPORTING
      ex_item  = es_ite
    EXCEPTIONS
      failure  = 1
      OTHERS   = 2.
* Carrega área de memória para calcular o imposto do Documnto de Compra posteriormente.
  ASSIGN ('(SAPLMEPO)ekpo') TO <ekpo>.
  ASSIGN ('(SAPLMEPO)ekko') TO <ekko>.
  ASSIGN ('(SAPLMEPO)lfa1') TO <lfa1>.

  SELECT SINGLE * FROM ekpo INTO <ekpo>
  WHERE ebeln EQ ie_param-ebeln
    AND ebelp EQ ie_param-ebelp
    AND loekz EQ space.

  SELECT SINGLE * FROM ekko INTO <ekko> WHERE ebeln EQ ie_param-ebeln.
* Verifica se o código não está marcado para elimonação.
  IF <ekpo>-loekz = space.

    IF <ekpo>-bednr IS NOT INITIAL.
      <ekpo>-bednr = | { <ekpo>-bednr ALPHA = IN WIDTH = 10 } |.
      SELECT SINGLE * FROM lfa1 INTO <lfa1> WHERE lifnr = <ekpo>-bednr.
* Se não existir fornecedor por linha
      IF sy-subrc NE 0.
        SELECT SINGLE * FROM lfa1 INTO <lfa1> WHERE lifnr = <ekko>-lifnr.

      ENDIF.

    ELSE.
      SELECT SINGLE * FROM lfa1 INTO <lfa1> WHERE lifnr = <ekko>-lifnr.

    ENDIF.

    SELECT * FROM prcd_elements INTO TABLE t_konv WHERE knumv = <ekko>-knumv.
* Carrega área de memória para calcular o imposto do Documnto de Compra posteriormente.
    ASSIGN ('(SAPLMEPO)fc_vorga') TO <vorga>.
    ASSIGN ('(SAPLMEPO)cva_en')   TO <cva>.
    ASSIGN ('(SAPLMEPO)tkomv[]')  TO <konv>.
    <vorga> = <cva>.
* Cálculo do imposto do Item do Documnto de Compra.
    PERFORM kond_taxes(saplmepo) USING 'D' abap_on.
* Carrega o valor do imposto do Documnto de Compra caculado.
    ASSIGN ('(SAPLMEPO)taxcom-WMWST') TO <wmwst>.

  ELSE.
* Carrega o valor vazio do imposto do Documnto de Compra.
    ASSIGN ('TAXCOM-WMWST') TO <wmwst>.

  ENDIF.

  ee_param = ie_param.

  IF ie_param-meng_ori GT 0.
    IF ie_param-bprme CS 'TO'.
      DATA(vl_fator) = 1000.

    ELSE.
      vl_fator = 1.

    ENDIF.

    lv_vlr_unit_brt = ( ( <wmwst> + ie_param-vlr_tot_liq ) / ie_param-meng_ori ) * vl_fator.

  ENDIF.

  ee_param-qtde_fat  = ee_param-qtde_fat    / vl_fator. "*-US192338-06.10.2025-#192338-JT
  ee_param-qtremfinal = ee_param-qtremfinal / vl_fator. "*-US192338-06.10.2025-#192338-JT
  ee_param-kbetr     = lv_vlr_unit_brt.
  ee_param-kbetr_imp = <wmwst>.
  ee_param-kbetr_tot = ( <wmwst> + ie_param-vlr_tot_liq ).
* Verifica se o recebimento foi concluído.
  IF ie_param-elikz = abap_on. "Remessa final
    ee_param-vlremfinal = ee_param-kbetr_tot - ee_param-kbetr_fat.
    ee_param-kbetr_sal  = ee_param-kbetr_tot - ee_param-kbetr_fat - ee_param-vlremfinal.

  ELSE.
    ee_param-kbetr_sal = ee_param-kbetr_tot - ee_param-kbetr_fat.

  ENDIF.

ENDFUNCTION.

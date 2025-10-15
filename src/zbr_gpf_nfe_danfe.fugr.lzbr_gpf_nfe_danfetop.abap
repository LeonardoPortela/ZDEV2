FUNCTION-POOL zbr_gpf_nfe_danfe MESSAGE-ID zbrnfe.


TYPE-POOLS: pbr99, ibco2.

TABLES: stxh.

*--------------------------------------------------------------------*
* TABLES
*--------------------------------------------------------------------*
TABLES: vbco3,
        vbdka,
        vbdpa.

*--------------------------------------------------------------------*
* TYPES
*--------------------------------------------------------------------*
DATA: BEGIN OF tvbdpa OCCURS 0.        "Internal table for items
        INCLUDE STRUCTURE vbdpa.
DATA: END OF tvbdpa.

DATA: BEGIN OF tkomcon OCCURS 50.
        INCLUDE STRUCTURE conf_out.
DATA: END   OF tkomcon.

****** Grupos de impostos ********************
* ICMS
CONSTANTS c_icms TYPE j_1btaxgrp VALUE 'ICMS'.
* Sub.trib.
CONSTANTS c_icst TYPE j_1btaxgrp VALUE 'ICST'.
* IPI
CONSTANTS c_ipi  TYPE j_1btaxgrp VALUE 'IPI'.
* ISS
CONSTANTS c_iss  TYPE j_1btaxgrp VALUE 'ISS'.
* Complemento do ICMS
CONSTANTS c_icop TYPE j_1btaxgrp VALUE 'ICOP'.
* ICMS sobre frete
CONSTANTS c_icfr TYPE j_1btaxgrp VALUE 'ICFR'.
* Substituição tributária sobre frete
CONSTANTS c_icfs TYPE j_1btaxgrp VALUE 'ICFS'.
* Reembolso substituição tributária
CONSTANTS c_rbst TYPE j_1btaxgrp VALUE 'RBST'.
* Reembolso ICMS
CONSTANTS c_rbic TYPE j_1btaxgrp VALUE 'RBIC'.
* ISS no local em que é fornecido o serviço
CONSTANTS c_isss TYPE j_1btaxgrp VALUE 'ISSS'.
* ISS no local do fornecedor de serviços
CONSTANTS c_issp TYPE j_1btaxgrp VALUE 'ISSP'.
* PIS
CONSTANTS c_pis  TYPE j_1btaxgrp VALUE 'PIS'.
* COFINS
CONSTANTS c_cofi TYPE j_1btaxgrp VALUE 'COFI'.

* IMPOSTOS
CONSTANTS c_imp_pis    TYPE j_1bnfstx-taxtyp VALUE 'ZPIS'.
CONSTANTS c_imp_cofins TYPE j_1bnfstx-taxtyp VALUE 'ZCOF'.

CONSTANTS c_iczf TYPE J_1BNFSTX-TAXTYP VALUE 'ICZF'.

* Numero ferente a totalizador.
CONSTANTS c_num_tot TYPE zbrnfe_danfe_item-num VALUE '999999'.
CONSTANTS c_num_cab TYPE zbrnfe_danfe_item-num VALUE '000000'.

CONSTANTS c_transp_frete TYPE j_1bnfnad-parvw VALUE '99'.

TYPES BEGIN OF tp_parceiro.
        INCLUDE STRUCTURE j_1bnfnad.
TYPES   dados TYPE j_1binnad.
TYPES END OF tp_parceiro.

TYPES:
  BEGIN OF tp_awkey,
    awkey TYPE bkpf-awkey,
    vbeln TYPE vbrp-vbeln,
  END OF tp_awkey.

TYPES:
  BEGIN OF tp_vbrk,
    vbeln TYPE vbrk-vbeln,
    fkart TYPE vbrk-fkart,
  END OF tp_vbrk.

TYPES:
  BEGIN OF tp_vbrp,
    vbeln TYPE vbrp-vbeln,
    posnr TYPE vbrp-posnr,
    aubel TYPE vbrp-aubel,
    aupos TYPE vbrp-aupos,
    vgbel TYPE vbrp-vgbel,
  END OF tp_vbrp.

TYPES:
  BEGIN OF tp_vbpa,
    kunnr TYPE vbpa-kunnr,
  END OF tp_vbpa.

TYPES:
  BEGIN OF tp_lotes,
    matnr TYPE mcha-matnr,
    werks TYPE mcha-werks,
    charg TYPE mcha-charg,
    lote  TYPE mcha-charg,
  END OF tp_lotes.


* Estrutura tabela CNAE
TYPES: BEGIN OF t7brb1_type,
        econa TYPE t7brb1-econa,
        endda TYPE t7brb1-endda,
      END OF t7brb1_type.

*TYPES: BEGIN OF flapu_types,
*          z_matnr   TYPE zmmt008-z_matnr,
*          z_flapu   TYPE zmmt008-z_flapu,
*          z_aerosol TYPE zmmt008-z_aerossol,
*       END OF flapu_types.

DATA: v_inflamavel(1),
      v_inflamavelpeso   TYPE string.
DATA: v_aerossol(1),
      v_aerossolpeso     TYPE string.

"DATA tl_flapu         TYPE STANDARD TABLE OF flapu_types WITH HEADER LINE.

*** Cabeçalho da nota fiscal
DATA t_j_1bnfdoc TYPE j_1bnfdoc.
*** Nota fiscal eletrônica: status atual
DATA t_j_1bnfe_active TYPE j_1bnfe_active.
*** Parceiros nota fiscal
DATA t_j_1bnfnad TYPE TABLE OF j_1bnfnad WITH HEADER LINE.
*** Local de negócios
DATA t_j_1bbranch TYPE j_1bbranch.
*** Partidas individuais da nota fiscal
DATA t_j_1bnflin TYPE STANDARD TABLE OF j_1bnflin WITH HEADER LINE.
*** Mensagens da Nota Fiscal
DATA t_j_1bnfftx TYPE TABLE OF j_1bnfftx WITH HEADER LINE.
*** Dados Acumulados
DATA t_j_1bindocdx TYPE j_1bindocdx.
*** Impostos por item
DATA t_j_1bnfstx TYPE TABLE OF j_1bnfstx WITH HEADER LINE.
*** Documentos de referencia
DATA t_j_1bnfref TYPE TABLE OF j_1bnfref WITH HEADER LINE.
*** Parceiros - Detalhes
DATA t_parceiro TYPE TABLE OF tp_parceiro WITH HEADER LINE.

DATA: BEGIN OF wk_item_add OCCURS 0.
        INCLUDE STRUCTURE j_1binlin.
DATA: END OF wk_item_add.

DATA: BEGIN OF wk_header_add.
        INCLUDE STRUCTURE j_1bindoc.
DATA: END OF wk_header_add.

DATA t_msg_ncm TYPE TABLE OF zbrnfe_danfe_ncm WITH HEADER LINE.
DATA lw_msg_ncm TYPE zbrnfe_danfe_ncm.

DATA lw_vttk TYPE vttk.

*** Parceiro recebedor - detalhe
DATA t_parc_recebedor TYPE j_1binnad.
*** Dados de endereco da filial
DATA g_sadr_branch TYPE sadr.
DATA g_addr_branch TYPE addr1_val.

*** Nome do módulo de função
DATA ls_funcname TYPE rs38l_fnam.
*** Empresas
DATA t_t001w TYPE t001w.
*** Endereços (administração de endereços central)
DATA t_adrc TYPE adrc.
*** Tipos de impostos
DATA t_j_1baj TYPE TABLE OF j_1baj WITH HEADER LINE.

*** DANFE - Dados de cabeçalho
DATA t_danfe_cabecalho TYPE STANDARD TABLE OF zbrnfe_danfe_cabecalho
                       WITH HEADER LINE.
*** DANFE - Dados de Item
DATA t_zbrnfe_danfe_item TYPE STANDARD TABLE OF zbrnfe_danfe_item
                         WITH HEADER LINE.
*** DANFE - Descrição de Item Múltiplas Linhas
DATA t_zbrnfe_danfe_item_desc TYPE STANDARD TABLE OF
       zbrnfe_danfe_item_desc WITH HEADER LINE.
*** DANFE - Dados de fatura
DATA t_zbrnfe_danfe_fatura TYPE STANDARD TABLE OF zbrnfe_danfe_fatura
                           WITH HEADER LINE.
*** DANFE - Dados CFOP
DATA t_zbrnfe_danfe_cfop TYPE STANDARD TABLE OF zbrnfe_danfe_cfop
                         WITH HEADER LINE.
*** DANFE - Dados adicionais
DATA t_zbrnfe_danfe_dados_adic TYPE STANDARD TABLE OF
       zbrnfe_danfe_dados_adic WITH HEADER LINE.

*** Estruturas para busca de CNPJ
DATA: lc_address     TYPE sadr,
      lc_branch_data TYPE j_1bbranch,
      lc_cgc_number  TYPE j_1bwfield-cgc_number,
      lc_address1    TYPE addr1_val,
      lc_cgc_aux     TYPE pbr99_cgc.

*** Descrição do CFOP
DATA is_cfotxt TYPE j_1bagt-cfotxt.

DATA li_date TYPE sy-datum.
DATA li_days TYPE i.
DATA le_date TYPE sy-datum.

*** Smart Forms: estrutura de controle
DATA g_control_parameters TYPE ssfctrlop.
DATA g_output_options     TYPE ssfcompop.

* Documentos referenciados
DATA gt_bkpf TYPE TABLE OF bkpf WITH HEADER LINE.
* Partidas
DATA gt_bseg TYPE TABLE OF bseg WITH HEADER LINE.
* Auxiliar para documentos referenciados
DATA gt_awkey TYPE TABLE OF tp_awkey WITH HEADER LINE.
* Pedidos cliente
DATA gt_vbrp TYPE TABLE OF tp_vbrp WITH HEADER LINE.
* Parceiro
DATA gt_vbpa TYPE TABLE OF tp_vbpa WITH HEADER LINE.
* Contador de linha de dados adicionais
DATA g_linha TYPE zbrnfe_danfe_dados_adic-linha.
* Total de descontos incondicionais.
DATA g_netdis TYPE j_1bnflin-netdis.
* Fornecedor
DATA gt_fornecedor TYPE TABLE OF lfa1 WITH HEADER LINE.
* Fluxo SD
DATA gt_vbfa TYPE TABLE OF vbfa WITH HEADER LINE.
* Faturamento
DATA gt_vbrk TYPE TABLE OF tp_vbrk WITH HEADER LINE.


DATA t_pdf        TYPE TABLE OF tline. "PDF Gerado
DATA l_pdf_line   LIKE LINE OF t_pdf.  "Linha do Pdf

* Field symbols para uso geral
FIELD-SYMBOLS: <f1> TYPE any,
               <f2> TYPE any,
               <f3> TYPE any,
               <f4> TYPE any,
               <f5> TYPE any,
               <f6> TYPE any,
               <f7> TYPE any,
               <f8> TYPE any.
* NFE em Contingencia
DATA g_contingencia.

DATA g_vkorg TYPE vbrk-vkorg.

* Doc. Referencia
DATA t_doc_ref TYPE j_1bnfdoc.
DATA t_nfe_ref TYPE j_1bnfe_active.

DATA: wg_xml_sefaz TYPE ZNFE_XML_SEFAZ_auth.

TABLES: vbrp,
        vbak,
        vbap,
        vbrk,
        vbfa,
        konv,
        knvv,
        j_1bnfdoc,
        j_1bnflin,
        j_1btxic3,
        j_1bnfnad,
        kna1,
        adrc,
        vbpa.

DATA t_txt_fatura TYPE STANDARD TABLE OF tline WITH HEADER LINE.

DATA da_mess LIKE vbfs OCCURS 0 WITH HEADER LINE.


CONSTANTS: c_zf2b TYPE c LENGTH 4 VALUE 'ZF2B',
           c_i103 TYPE c LENGTH 4 VALUE 'I103',
           c_i133 TYPE c LENGTH 4 VALUE 'I133',
           c_01   TYPE c LENGTH 2 VALUE '01'.


CONSTANTS:
  c_zvic       TYPE char4      VALUE 'ZVIC',
  c_d102       TYPE char4      VALUE 'D102',
  c_dist_03    TYPE char3      VALUE '03',
  c_lj_eudor   TYPE char2      VALUE 'JL'.

RANGES: t_cfop_exp FOR tvarvc-low.
DATA:   t_uom_map TYPE TABLE OF j_1bnfe_uom_map WITH HEADER LINE.

SELECTION-SCREEN BEGIN OF SCREEN 9100 AS WINDOW TITLE text-t01.
*Tipo de contingência
PARAMETERS: p_tp_con TYPE zbrnfe_tipo_contingencia OBLIGATORY.
SELECTION-SCREEN END OF SCREEN 9100.


FORM ordenadanfeintemdesc  TABLES pt_zbrnfe_danfe_item      STRUCTURE zbrnfe_danfe_item
                                  pt_zbrnfe_danfe_item_desc STRUCTURE zbrnfe_danfe_item_desc.
  LOOP AT pt_zbrnfe_danfe_item.
    IF pt_zbrnfe_danfe_item-descricao(1) = '*'.
      CONCATENATE '1' pt_zbrnfe_danfe_item-descricao INTO pt_zbrnfe_danfe_item-descricao.

    ELSEIF pt_zbrnfe_danfe_item-descricao(1) = '#'.
      CONCATENATE '2' pt_zbrnfe_danfe_item-descricao INTO pt_zbrnfe_danfe_item-descricao.

    ELSE.
      CONCATENATE '3' pt_zbrnfe_danfe_item-descricao INTO pt_zbrnfe_danfe_item-descricao.
    ENDIF.
    MODIFY pt_zbrnfe_danfe_item INDEX sy-tabix.
  ENDLOOP.

  LOOP AT pt_zbrnfe_danfe_item_desc.
    IF pt_zbrnfe_danfe_item_desc-descricao(1) = '*'.
      CONCATENATE '1' pt_zbrnfe_danfe_item_desc-descricao INTO pt_zbrnfe_danfe_item_desc-descricao.

    ELSEIF pt_zbrnfe_danfe_item_desc-descricao(1) = '#'.
      CONCATENATE '2' pt_zbrnfe_danfe_item_desc-descricao INTO pt_zbrnfe_danfe_item_desc-descricao.

    ELSE.
      CONCATENATE '3' pt_zbrnfe_danfe_item_desc-descricao INTO pt_zbrnfe_danfe_item_desc-descricao.
    ENDIF.
    MODIFY pt_zbrnfe_danfe_item_desc INDEX sy-tabix.
  ENDLOOP.
  SORT pt_zbrnfe_danfe_item        BY descricao.
  SORT pt_zbrnfe_danfe_item_desc   BY descricao.

  LOOP AT pt_zbrnfe_danfe_item.
    pt_zbrnfe_danfe_item-descricao = pt_zbrnfe_danfe_item-descricao+1.
    MODIFY pt_zbrnfe_danfe_item INDEX sy-tabix.
  ENDLOOP.

  LOOP AT pt_zbrnfe_danfe_item_desc.
    pt_zbrnfe_danfe_item_desc-descricao = pt_zbrnfe_danfe_item_desc-descricao+1.
    MODIFY pt_zbrnfe_danfe_item_desc INDEX sy-tabix.
  ENDLOOP.
ENDFORM.                    " ORDENADANFEINTEMDESC

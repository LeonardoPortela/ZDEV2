function z_monta_xml.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(XML_IN) TYPE  J1B_NF_XML_HEADER OPTIONAL
*"     REFERENCE(XML_HEAD_TAB) TYPE  J1B_NF_XML_HEADER_TAB OPTIONAL
*"     REFERENCE(XML_ITEM_TAB) TYPE  J1B_NF_XML_ITEM_TAB OPTIONAL
*"     REFERENCE(XML_BATCH) TYPE  J1B_NF_XML_J_TAB OPTIONAL
*"     REFERENCE(XML_REF) TYPE  J1B_NF_XML_B12_TAB OPTIONAL
*"     REFERENCE(XML_DUP) TYPE  J1B_NF_XML_U2_TAB OPTIONAL
*"     REFERENCE(XML_VOL) TYPE  J1B_NF_XML_T4_TAB OPTIONAL
*"     REFERENCE(XML_IMP) TYPE  J1B_NF_XML_H4_TAB OPTIONAL
*"     REFERENCE(XML_EXT1) TYPE  J1B_NF_XML_EXTENSION1_TAB OPTIONAL
*"     REFERENCE(XML_EXT2) TYPE  J1B_NF_XML_EXTENSION2_TAB OPTIONAL
*"     REFERENCE(GS_NFDOC) TYPE  J_1BNFDOC OPTIONAL
*"  TABLES
*"      IT_XML STRUCTURE  ZEXML OPTIONAL
*"  EXCEPTIONS
*"      COMMUNICATION_FAILURE
*"      SYSTEM_FAILURE
*"----------------------------------------------------------------------

  data: corte           type j_1bdocdat,
        p_emite_ciot    type char01,
        wa_j_1bnfdoc    type j_1bnfdoc,
        wa_j_1bnflin    type j_1bnflin,
        wa_j1bnflin_aux type j_1bnflin,
        p_tp_forne      type ztp_fornecimento,
        vg_vstel        type char01,
        p_tknum         type tknum,
        wl_zlest0061    type zlest0061,
        st_xml_ext2     type j1b_nf_xml_extension2,
        wl_j1bnfdoc     type j_1bnfdoc,
        vl_docnum       type j_1bdocnum.


* Limpa Tabelas Globais
  perform z_limpa_globais.

  IMPORT IT_J1B_NF_XML_BADI_ITEM FROM MEMORY ID 'IT_J1B_NF_XML_BADI_ITEM'.

  clear: vl_docnum, st_xml_ext2.
  "Busca qual ambiente esta logado
  clear vg_ambiente.
  perform get_ambiente changing vg_ambiente.

  if xml_in is INITIAL and gs_nfdoc is NOT INITIAL.
    xml_in-mod = gs_nfdoc-model.
  endif.

  perform get_versao_xml.

  case xml_in-mod.

    when: '55'.  "Nota Fiscal Eletronica - Modelo 55
      perform monta_xml_nfe tables xml_head_tab xml_item_tab xml_batch xml_ref xml_dup
                                   xml_vol xml_imp xml_ext1 xml_ext2 it_xml
                             using xml_in.

    when: '57'. "Conhecimento Eletronico - Modelo 57

      vl_docnum  = gs_nfdoc-docnum.
      if vl_docnum is initial.
        read table xml_ext2 into st_xml_ext2 index 1.
        vl_docnum = st_xml_ext2-docnum.
      endif.

      select single * into wa_j_1bnfdoc from j_1bnfdoc where docnum eq vl_docnum.
      select single * into wa_j_1bnflin from j_1bnflin where docnum eq vl_docnum.

      case wa_j_1bnflin-reftyp.

        when: 'ZW'.

          call function 'Z_MONTA_XML_CTE'
            exporting
              p_cte_avulso = vl_docnum
              p_envia_xi   = 'X'
            tables
              it_cancel    = xml_ext2.

        when others.

          CLEAR: wl_zlest0061.
          "Verificar se o docnum esta gravado na tabela do aquaviário.
          select single * from zlest0061 into wl_zlest0061 where docnum eq vl_docnum.

          if sy-subrc ne 0.
            clear: wl_j1bnfdoc.
            select single *
              from j_1bnfdoc into wl_j1bnfdoc
             where docnum = vl_docnum
               and doctyp = '2'.

            if ( sy-subrc = 0 ) and ( wl_j1bnfdoc-docref is not initial ).
              clear: wl_zlest0061.
              select single * from zlest0061 into wl_zlest0061 where docnum eq wl_j1bnfdoc-docref.
            endif.
          endif.

          if not ( wl_zlest0061 is initial ) and ( wa_j_1bnflin-reftyp eq 'BI' ).

            call function 'Z_MONTA_XML_CTE'
              exporting
                p_cte_avulso = vl_docnum
                p_envia_xi   = 'X'
              tables
                it_cancel    = xml_ext2.

          else.

            call function 'Z_REMETENTE_MERCADORIA_CTE'
              exporting
                p_docnum   = vl_docnum
              changing
                p_bukrs    = wa_j_1bnfdoc-bukrs
                p_parid    = wa_j_1bnfdoc-parid
                p_partyp   = wa_j_1bnfdoc-partyp
                p_tp_forne = p_tp_forne
                vg_vstel   = vg_vstel
                p_tknum    = p_tknum.

            call function 'Z_CIOT_EMPRESA_PARCEIRO'
              exporting
                p_empresa    = wa_j_1bnfdoc-bukrs
                p_parid      = wa_j_1bnfdoc-parid
                p_partyp     = wa_j_1bnfdoc-partyp
                p_dt_posicao = wa_j_1bnfdoc-docdat
                p_tknum      = p_tknum
              importing
                p_emite      = p_emite_ciot.
            " Emite CIOT ou Aviso Recebimento
            if ( p_emite_ciot is not initial ) or ( p_tp_forne eq 'A' ) or ( not vg_vstel is initial ).
              call function 'Z_MONTA_XML_CTE'
                exporting
                  p_cte_avulso = vl_docnum
                  p_envia_xi   = 'X'
                tables
                  it_cancel    = xml_ext2.
            else.
              "Conhecimento de Transporte eletrônico (CT-e) Modelo 57
              if gs_nfdoc-docnum is not initial.
                xml_in-docnum = gs_nfdoc-docnum.
              endif.
              perform monta_xml_cte tables xml_head_tab xml_item_tab xml_batch xml_ref xml_dup
                                           xml_vol xml_imp xml_ext1 xml_ext2 it_xml
                                     using xml_in.
            endif.
          endif.
      endcase.
  endcase.

endfunction.

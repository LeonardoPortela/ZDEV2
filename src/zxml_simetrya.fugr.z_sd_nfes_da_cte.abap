function z_sd_nfes_da_cte.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(MP_DOCNUM) LIKE  J_1BNFDOC-DOCNUM
*"  TABLES
*"      MT_PARTNER STRUCTURE  J_1BNFNAD OPTIONAL
*"      MT_ITEM STRUCTURE  J_1BNFLIN OPTIONAL
*"      MT_ITEM_TAX STRUCTURE  J_1BNFSTX OPTIONAL
*"      MT_HEADER_MSG STRUCTURE  J_1BNFFTX OPTIONAL
*"      MT_REFER_MSG STRUCTURE  J_1BNFREF OPTIONAL
*"      MT_VBRP STRUCTURE  VBRP OPTIONAL
*"      MT_VBAK STRUCTURE  VBAK OPTIONAL
*"      MT_VTTP STRUCTURE  VTTP OPTIONAL
*"      MT_HEADRE_NOTA_NFE STRUCTURE  J_1BNFDOC OPTIONAL
*"      MT_ITEM_NOTA STRUCTURE  J_1BNFLIN OPTIONAL
*"      MT_PARTNER_P STRUCTURE  J_1BNFNAD OPTIONAL
*"      MT_ITEM_TAX_P STRUCTURE  J_1BNFSTX OPTIONAL
*"      MT_HEADER_MSG_P STRUCTURE  J_1BNFFTX OPTIONAL
*"      MT_REFER_MSG_P STRUCTURE  J_1BNFREF OPTIONAL
*"      MT_NFE_ACTIVE STRUCTURE  J_1BNFE_ACTIVE OPTIONAL
*"  CHANGING
*"     VALUE(MR_VTTK) TYPE  VTTK OPTIONAL
*"     VALUE(MR_HEADER_NOTA_CTE) TYPE  J_1BNFDOC OPTIONAL
*"     VALUE(MR_VBAK) TYPE  VBAK OPTIONAL
*"     VALUE(MR_VBRK) TYPE  VBRK OPTIONAL
*"     VALUE(MR_LFA1) TYPE  LFA1 OPTIONAL
*"     VALUE(MR_TTDS) TYPE  TTDS OPTIONAL
*"----------------------------------------------------------------------

  data: mi_vbfap_docm type table of y_vbfa_docm,
        mi_vbfap      type table of y_vbfa,
        wa_vbfa_docm  type y_vbfa_docm,
        wa_vbfap      type table of y_vbfa,
        wa_item_nota  type j_1bnfref,
        wa_nfe_active type j_1bnfe_active,
        mi_lips       type table of y_lips,
        mi_ekbe       type table of y_ekbe,
        mi_rbkp       type table of y_rbkp,
        wa_rbkp       type y_rbkp,
        mi_reflin     type table of y_reflin,
        wa_reflin     type y_reflin,
        wa_vbrp       type vbrp,
        vl_mblnr      type mblnr,
        vl_tabix      type sy-tabix,
        wa_vttp       type vttp,
        wa_j_1bnfnad  type j_1bnfnad,
        it_vbpa       type table of vbpa,
        wa_vbpa       type vbpa,
        wa_kna1       type kna1.

  data: it_nf_aviso_receb type table of zmmt_ee_zgr_docs with header line.

* Obter os dados na nota
  call function 'J_1B_NF_DOCUMENT_READ'
    exporting
      doc_number         = mp_docnum
    importing
      doc_header         = mr_header_nota_cte
    tables
      doc_partner        = mt_partner
      doc_item           = mt_item
      doc_item_tax       = mt_item_tax
      doc_header_msg     = mt_header_msg
      doc_refer_msg      = mt_refer_msg
    exceptions
      document_not_found = 1
      docum_lock         = 2
      others             = 3.

  check sy-subrc eq 0.

* Obter os dados da fatura (item) a partir da nota fiscal de serviço
  refresh mt_vbrp.
  select *
    from vbrp into table mt_vbrp
     for all entries in mt_item
    where vbeln = mt_item-refkey(10)
      and posnr = mt_item-refitm.

  check sy-subrc eq 0.

* Obter os dados da fatura (item) a partir da nota fiscal de serviço
  read table mt_vbrp into wa_vbrp index 1.

* Obter os dados Determinação de dados da ordem referentes a prestação
* do serviço de transporte.
  refresh mt_vbak.
  select * into table mt_vbak
           from vbak
       for all entries in mt_vbrp
          where vbeln = mt_vbrp-aubel.

  check sy-subrc eq 0.

* Determinação de dados do documento de transporte referente a venda
* de produtos.
  read table mt_vbak into mr_vbak index 1.

  select single * into mr_vbrk
    from vbrk
   where vbeln = wa_vbrp-vbeln.

* Cabeçalho do documento de transporte
  clear mr_vttk.

  select single * into mr_vttk from vttk where tknum = mr_vbak-tknum.
  check sy-subrc = 0.

  select single * into mr_lfa1 from lfa1 where lifnr = mr_vttk-tdlnr.
  check sy-subrc eq 0.

* Determinar os dados referente ao cenário de venda de produto, referente
* ao documento de transporte de produtos.
* Itens do documento de transporte
  refresh mt_vttp.

  select * into table mt_vttp from vttp where tknum = mr_vttk-tknum.
  check sy-subrc eq 0.

  select single * into mr_ttds from ttds where tplst = mr_vttk-tplst.
  check sy-subrc eq 0.

* Documentos de fatura referente as remessas do documento de transporte
  if mr_vttk-abfer = c_1 or   "Transporte de partida com carga
     mr_vttk-abfer = c_3.     "Transporte de partida vazio

    if mr_vttk-shtyp = c_z020.         "Transferência

** Obter o documento de material
*      REFRESH mi_vbfap_docm.
*
*      SELECT fa~vbeln fa~mjahr fa~posnn fa~vbeln INTO TABLE mi_vbfap_docm
*        FROM vbfa AS fa
*        INNER JOIN mseg AS ms ON ms~smbln EQ fa~vbeln AND ms~sjahr = fa~mjahr
*         FOR ALL ENTRIES IN mt_vttp
*       WHERE fa~vbelv   EQ mt_vttp-vbeln
*         AND fa~vbtyp_v EQ c_j       "Entrega
*         AND fa~vbtyp_n EQ c_r       "Movimento de Mercadoria
*         AND ms~bwart   NE c_864.
*
** Agrupar o documento de material e o ano em um mesmo campo.
*      LOOP AT mi_vbfap_docm INTO wa_vbfa_docm.
*        CONCATENATE wa_vbfa_docm-vbeln_35  wa_vbfa_docm-mjahr INTO wa_vbfa_docm-vbeln_35.
*        MODIFY mi_vbfap_docm FROM wa_vbfa_docm INDEX sy-tabix TRANSPORTING vbeln_35.
*      ENDLOOP.

* Obter o documento de material
      refresh mi_vbfap_docm.
      select vbeln mjahr posnn vbeln into table mi_vbfap_docm
        from vbfa
         for all entries in mt_vttp
       where vbelv = mt_vttp-vbeln
         and vbtyp_v = c_j   "Entrega
         and vbtyp_n = c_r.  "Movimento de Mercadoria

* Eliminar o documento de material estornado.
      loop at mi_vbfap_docm into wa_vbfa_docm.
        vl_tabix = sy-tabix.
        clear vl_mblnr.
        select mblnr into vl_mblnr
          from mseg
         up to 1 rows
         where smbln = wa_vbfa_docm-vbeln
           and sjahr = wa_vbfa_docm-mjahr
           and bwart = c_864.   "Movimento de estorno
        endselect.

        if sy-subrc eq 0.
          delete mi_vbfap_docm index vl_tabix.
        else.
* Agrupar o documento de material e o ano em um mesmo campo.
          concatenate wa_vbfa_docm-vbeln_35  wa_vbfa_docm-mjahr into wa_vbfa_docm-vbeln_35.
          modify mi_vbfap_docm from wa_vbfa_docm index vl_tabix transporting vbeln_35.
        endif.
      endloop.

* Obter a nota fiscal
      refresh mt_item_nota.
      select *
        into corresponding fields of table mt_item_nota
        from j_1bnflin
         for all entries in mi_vbfap_docm
       where refkey = mi_vbfap_docm-vbeln_35
         and refitm = mi_vbfap_docm-posnn
         and reftyp = c_md. "Documento de material

*---> 04/07/2023 - Migração S4 - WS
  SORT mt_item_nota.
*<--- 04/07/2023 - Migração S4 - WS
      delete adjacent duplicates from mt_item_nota.

    elseif mr_vttk-shtyp = c_z018.  "Venda Triangular

      refresh mi_vbfap.

      select v~vbeln v~posnn v~vbeln into table mi_vbfap "#EC CI_DB_OPERATION_OK[2768887]
        from vbfa as v
       inner join vbrk as k on k~vbeln eq v~vbeln
       inner join vbrp as p on p~vbeln eq v~vbeln and p~posnr eq v~posnn
       inner join vbak as e on e~vbeln eq p~aubel
         for all entries in mt_vttp
       where v~vbelv   = mt_vttp-vbeln
         and v~vbtyp_v = c_j   "Entrega
         and v~vbtyp_n = c_m   "Fatura
         and k~fksto  <> c_x
         and e~auart   = c_zrem AND K~DRAFT = SPACE  and p~DRAFT = SPACE .

      if sy-subrc eq 0.

        refresh mt_item_nota.

        select *
          into corresponding fields of table mt_item_nota
          from j_1bnflin
           for all entries in mi_vbfap
         where refkey = mi_vbfap-vbeln_35
           and refitm = mi_vbfap-posnn
           and reftyp = c_bi.           "Faturamento

*---> 04/07/2023 - Migração S4 - WS
  SORT mt_item_nota.
*<--- 04/07/2023 - Migração S4 - WS
        delete adjacent duplicates from mt_item_nota.

      endif.

    else.
* Não é transferência
      refresh mi_vbfap.
      select v~vbeln v~posnn v~vbeln into table mi_vbfap
        from vbfa as v
       inner join vbrk as k on k~vbeln = v~vbeln
         for all entries in mt_vttp
       where v~vbelv = mt_vttp-vbeln
         and v~vbtyp_v = c_j       "Entrega
         and v~vbtyp_n = c_m       "Fatura
         and k~fksto <> c_x AND K~DRAFT = SPACE .

      if sy-subrc eq 0.

        refresh mt_item_nota.

        select *
          into corresponding fields of table mt_item_nota
          from j_1bnflin
           for all entries in mi_vbfap
         where refkey = mi_vbfap-vbeln_35
           and refitm = mi_vbfap-posnn
           and reftyp = c_bi.           "Faturamento

*---> 04/07/2023 - Migração S4 - WS
  SORT mt_item_nota.
*<--- 04/07/2023 - Migração S4 - WS
        delete adjacent duplicates from mt_item_nota.

      endif.
    endif.

  elseif ( mr_vttk-abfer = c_2 ) and ( mr_vttk-shtyp = c_z021 ).  "Receb. Frete Próprio

    select * into table it_nf_aviso_receb
      from zmmt_ee_zgr_docs
       for all entries in mt_vttp
     where av_vbeln eq mt_vttp-vbeln.
    delete it_nf_aviso_receb where docnum eq space.

    if it_nf_aviso_receb[] is NOT INITIAL.
      select * into table mt_item_nota
        from j_1bnflin
         for all entries in it_nf_aviso_receb
       where docnum eq it_nf_aviso_receb-docnum.
    endif.

  else.

* Documentos de remessa referente ao documento de transporte de produtos
    refresh mi_lips.
    select vbeln vgbel vgpos werks vbeln into table mi_lips
      from lips
       for all entries in mt_vttp
     where vbeln = mt_vttp-vbeln.

    check sy-subrc eq 0.

* Histórico de pedido de compras
    refresh mi_ekbe.
    select ebeln ebelp gjahr belnr xblnr
      into table mi_ekbe
      from ekbe
       for all entries in mi_lips
     where ebeln = mi_lips-vgbel
       and ebelp = mi_lips-vgpos+1(5)
       and xblnr = mi_lips-vbeln_16
       and bewtp = c_q.      "RE-L

* Determinar os invoices
    refresh mi_rbkp.
    select belnr gjahr
      into table mi_rbkp
      from rbkp
       for all entries in mi_ekbe
     where belnr = mi_ekbe-belnr
       and stblg = space.

* Ajustar o tipo de campo.
    refresh mt_item_nota.

    loop at mi_rbkp into wa_rbkp.
      concatenate wa_rbkp-belnr wa_rbkp-gjahr into wa_reflin-refkey.
      append wa_reflin to mi_reflin.
    endloop.

* Determinar os documentos de nota fiscais referentes ao produto
    refresh mt_item_nota.
    select *
      into corresponding fields of table mt_item_nota
      from j_1bnflin
       for all entries in mi_reflin
     where reftyp = c_li            "Logística: Revisão de faturas
       and refkey = mi_reflin-refkey.

*---> 04/07/2023 - Migração S4 - WS
  SORT mt_item_nota.
*<--- 04/07/2023 - Migração S4 - WS
    delete adjacent duplicates from mt_item_nota.
  endif.


* Definição de uma estrutura e tabelas locais para obtenção das notas fiscais
* dos produtos referentes a nota fiscal de
* Estrutura local.
  data: wk_header_p     type j_1bnfdoc,
        vl_cancel type j_1bnfe_active-cancel.

* Tabelas locais
  data: tl_header_p     type table of j_1bnfdoc,
        tl_partner_p    type table of j_1bnfnad,
        tl_item_p       type table of j_1bnflin,
        tl_item_tax_p   type table of j_1bnfstx,
        tl_header_msg_p type table of j_1bnfftx,
        tl_refer_msg_p  type table of j_1bnfref.

  loop at mt_item_nota into wa_item_nota.

* Desprezar notas canceladas
    select single cancel into vl_cancel
              from j_1bnfe_active
             where docnum = wa_item_nota-docnum.

    check vl_cancel is initial.

    clear: wk_header_p, wa_kna1.
    refresh: tl_partner_p,
             tl_item_p,
             tl_item_tax_p,
             tl_header_msg_p,
             tl_refer_msg_p.

* Obter os dados da nota fiscal de produto
    call function 'J_1B_NF_DOCUMENT_READ'
      exporting
        doc_number         = wa_item_nota-docnum
      importing
        doc_header         = wk_header_p
      tables
        doc_partner        = tl_partner_p
        doc_item           = tl_item_p
        doc_item_tax       = tl_item_tax_p
        doc_header_msg     = tl_header_msg_p
        doc_refer_msg      = tl_refer_msg_p
      exceptions
        document_not_found = 1
        docum_lock         = 2
        others             = 3.

    select single * into wa_nfe_active
      from j_1bnfe_active
     where docnum eq wk_header_p-docnum.

    append wa_nfe_active to mt_nfe_active.

* Salvar os dados das tabelas
    append wk_header_p              to mt_headre_nota_nfe.
    append lines of tl_partner_p    to mt_partner_p.
    "APPEND LINES OF tl_item_p       TO mt_item_nota.
    append lines of tl_item_tax_p   to mt_item_tax_p.
    append lines of tl_header_msg_p to mt_header_msg_p.
    append lines of tl_refer_msg_p  to mt_refer_msg_p.

    if mr_vttk-shtyp = c_z018.

      read table mt_vttp index 1 into wa_vttp.

      select * into corresponding fields of table it_vbpa
        from vbpa
       where vbeln eq wa_vttp-vbeln
         and parvw eq 'WE'.

      if sy-subrc is initial.

        read table it_vbpa into wa_vbpa index 1.

        call function 'Z_PARCEIRO_INFO'
          exporting
            p_parceiro = wa_vbpa-kunnr
            p_partype  = 'C'
          changing
            wa_info_c  = wa_kna1.

        move-corresponding wa_kna1 to wa_j_1bnfnad.
        wa_j_1bnfnad-docnum = wa_item_nota-docnum.
        wa_j_1bnfnad-parid = wa_vbpa-kunnr.
        wa_j_1bnfnad-parvw = 'WE'.
        append wa_j_1bnfnad to mt_partner_p.
      endif.

    endif.

  endloop.

endfunction.

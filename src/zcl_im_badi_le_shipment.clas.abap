class ZCL_IM_BADI_LE_SHIPMENT definition
  public
  final
  create public .

*"* public components of class ZCL_IM_BADI_LE_SHIPMENT
*"* do not include other source files here!!!
public section.

  interfaces IF_EX_BADI_LE_SHIPMENT .

  methods VER_MOV_INTERCOMPANY
    importing
      !P_VTTK type VTTK
      !I_NEW_VTTK type VTTKVB_TAB
    exporting
      !P_ADD03 type VTTK_ADD03
      !O_NEW_VTTK type VTTKVB_TAB .
  methods VER_EMITE_CIOT
    importing
      !WK_VTTK type VTTK
      !WA_VTRLP type VTRLP
    exporting
      !P_EMITE type CHAR01 .
  methods VER_PERMITE_ALTERACAO
    importing
      !WK_VTTK_ANT type VTTK
      !WK_VTTK_ATU type VTTK
    exceptions
      NAO_PERMITE .
protected section.
*"* protected components of class ZCL_IM_BADI_LE_SHIPMENT
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_IM_BADI_LE_SHIPMENT
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_IM_BADI_LE_SHIPMENT IMPLEMENTATION.


method if_ex_badi_le_shipment~at_save.

  constants: c_kappl(1) type c value 'F',
             c_kschl(4) type c value 'ZMRG',
             c_n(1)     type c value 'N', "Alteração Carta Frete "CSB
             c_x(1)     type c value 'X'. "Alteração Carta Frete "CSB

  type-pools: vt04.

  types :
     begin of y_vbfa,
             vbelv    type vbfa-vbelv,
             posnv    type vbfa-posnv,
             vbeln    type j_1bnflin-refkey,
             posnn    type j_1bnflin-itmnum,
             vbtyp_n  type vbfa-vbtyp_n,
             vbtyp_v  type vbfa-vbtyp_v,
     end of y_vbfa.

  data : wk_vbpa        type vbpavb,
         wa_vtrlp       type vtrlp,
         wk_vttk        type vttk,
         wk_vttko       type vttk, " Dados Antigos "CSB
         wk_vtrlk       type vtrlk,
         wk_vttp        type vttp,
         wk_912         type a912,              " Tolerância
         wk_konp        type konp,              " Aliquota de margens
         vl_agregado    type zlest0002-agregado,
         vl_placa       type zlest0002-pc_veiculo,
         vl_grpcta      type lfa1-ktokk,
         wk_0014        type zlest0014,
         vl_name1       type lfa1-name1,
         vl_lote        type zlest0013-lote,
         vl_vbfa        type y_vbfa,
         vl_cont        type i,
         vl_ctrl        type c,
         wk_matnr       type mara-matnr,
         vl_contrem     type i,
         wa_zlest0036   type zlest0036,
         vl_etapa       type char1,
         wa_vttp        type vttpvb.

  data: l_shipment_data  type vt04_shipment,
        e_result         type netwr_all,
        true             type c value 'X',
        wk_zlest0014     type zlest0014,
        v_motivo         type zlest0014-motivo,
        v_alterar        type zlest0014-alterar,
        cl_msg           type symsgid,
        nr_msg           type symsgno,  " Informações de Transporte CSB
        e_vbeln          type vttp-vbeln,
        e_werks          type lips-werks,
        wa_zsdt_depara_cen type zsdt_depara_cen,
        c_vbeln          type vttp-vbeln,
        c_werks          type lips-werks,
        it_vttp          type table of vttpvb,
        vl_campo         type char30 value '(SAPMV56A)XVTTP[]',
        p_emite          type char01.

  field-symbols <xvttp> type standard table.
  assign (vl_campo) to <xvttp>.

  "US 94987 - WPP - Fluxo onde não tem Pesagem OPUS
  read table im_shipments_at_save-new_vttk into DATA(wk_vttk_new) index 1.
  read table im_shipments_at_save-old_vttk into DATA(wk_vttk_old) index 1.

  if wk_vttk_new-sttbg NE wk_vttk_old-sttbg.

    IF wk_vttk_new-tknum is NOT INITIAL.
      DATA(lva_tknum) = wk_vttk_new-tknum.
    ELSE.
      lva_tknum = wk_vttk_old-tknum.
    ENDIF.

    IF wk_vttk_new-sttbg EQ ABAP_TRUE.
      DATA(_tp_status) = 'FE'.
    ELSE.
      _tp_status = 'AB'.
    ENDIF.

    CALL FUNCTION 'ZLES_SET_STATUS_OC_SEM_OPUS'
      EXPORTING
        i_tknum        = lva_tknum
        i_status       = _tp_status.
  ENDIF.
  "US 94987 - WPP - Fluxo onde não tem Pesagem OPUS


  check sy-tcode = 'VT01N' or sy-tcode = 'VT04' or sy-tcode = 'VT07'  or sy-tcode = 'VT02N'.

  "CHECK NOT sy-ucomm = 'YES'.

  cl_msg = 'ZLES'.
  if sy-ucomm eq 'YES'.
    nr_msg = '000'.
  else.
    nr_msg = '064'.
  endif.

  clear: wk_vbpa,  vl_agregado, vl_grpcta, vl_placa, vl_name1,  wk_0014, vl_lote, vl_vbfa, vl_cont, wk_zlest0014, wa_vttp .

* Dados do documento de transporte
  read table im_shipments_at_save-new_vttk into wk_vttk index 1.

  call method me->ver_mov_intercompany
    exporting
      p_vttk     = wk_vttk
      i_new_vttk = im_shipments_at_save-new_vttk
    importing
      p_add03    = wk_vttk-add03
      o_new_vttk = cha_shipments_at_save-new_vttk.

  " Valida autorização de alteração de conhecimento. CSB
  read table im_shipments_at_save-old_vttk into wk_vttko index 1.

  " Valida parceiro proprietário de veículo
  read table im_shipments_at_save-new_vbpa into wk_vbpa with key parvw = 'PV'.

  if sy-subrc is initial.
    call function 'Z_VERIFICA_CLI_FOR_CTA_MAT'
      exporting
        p_koart      = 'K'
        p_filial     = wk_vttk-tplst
        p_fornecedor = wk_vbpa-lifnr
      exceptions
        error        = 1
        branch       = 2
        others       = 3.

    if not sy-subrc is initial.
      message id sy-msgid type 'I' number sy-msgno with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      exit.
    endif.
  endif.

  call method me->ver_permite_alteracao
    exporting
      wk_vttk_ant = wk_vttko
      wk_vttk_atu = wk_vttk
    exceptions
      nao_permite = 1
      others      = 2.

  if not sy-subrc is initial.
    message id sy-msgid type sy-msgty number sy-msgno with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.

  clear: p_emite.

  "Verificação de emissão de CIOT para parceiro.
  if not ( im_shipments_at_save-new_vtrlp[] is initial ).
    read table im_shipments_at_save-new_vtrlp[] into wa_vtrlp index 1.
    call method me->ver_emite_ciot
      exporting
        wk_vttk  = wk_vttk
        wa_vtrlp = wa_vtrlp
      importing
        p_emite  = p_emite.
  endif.

  if ( p_emite is initial ) and ( wk_vttk-add03 eq '0000000001' ).

    if ( sy-tcode eq 'VT02N' ) and ( wk_vttko-exti2 ne wk_vttk-exti2 ).

      select single *
        into wk_zlest0014
        from zlest0014
       where tknum    eq wk_vttko-tknum
        and  conhec   eq wk_vttko-exti1
        and  ctafrete eq wk_vttko-exti2.

      if ( sy-subrc is initial ) and ( wk_zlest0014-alterar eq c_n ) .
        message e066(zles).
        exit.
        "MESSAGE ID cl_msg TYPE 'E' NUMBER nr_msg WITH 'Carta Frete não autorizada para modificação!' RAISING error_with_message.
      endif.


      if wk_zlest0014-alterar ne c_n. "N"
        " Busca a Filial e verifica se numero de carta frete já foi cadastrado
        select single vbeln
        into e_vbeln
        from vttp
        where tknum = wk_vttko-tknum.

        if not sy-subrc is initial.
          read table im_shipments_at_save-new_vttp[] into wa_vttp index 1.
          e_vbeln = wa_vttp-vbeln.
        endif.

        select single werks
        into  e_werks
        from lips
        where vbeln = e_vbeln.

        if wk_vttk-shtyp eq 'Z021'.

          select single * into wa_zsdt_depara_cen
            from zsdt_depara_cen
           where centrov_1 eq e_werks.

          if sy-subrc is initial.
            e_werks = wa_zsdt_depara_cen-centro_real.
          endif.

        endif.

        select single *
          into wk_zlest0014
          from zlest0014
          where ctafrete eq wk_vttk-exti2
          and  werks     eq e_werks.

        if ( sy-subrc is initial ) and  ( wk_zlest0014-motivo ne c_x ).
          message e067(zles).
          exit.
          "MESSAGE ID cl_msg TYPE 'E' NUMBER nr_msg WITH 'Carta Frete já Cadastrada Anteriormente.' RAISING error_with_message.
        endif.

      endif.
    endif.

    " Verifica se pode alterar.
    select single motivo alterar
    into (v_motivo , v_alterar )
    from zlest0014
    where ctafrete eq wk_vttk-exti2
    and  motivo    eq c_x.

    if ( v_motivo eq c_x ) and ( v_alterar ne c_n ) .

      clear: v_motivo, v_alterar.

      select single vbeln
      into e_vbeln
      from vttp
      where tknum = wk_vttko-tknum.

      if not sy-subrc is initial.
        read table im_shipments_at_save-new_vttp[] into wa_vttp index 1.
        e_vbeln = wa_vttp-vbeln.
      endif.

      select single werks
      into  e_werks
      from lips
      where vbeln = e_vbeln.

      if wk_vttk-shtyp eq 'Z021'.

        select single * into wa_zsdt_depara_cen
          from zsdt_depara_cen
         where centrov_1 eq e_werks.

        if sy-subrc is initial.
          e_werks = wa_zsdt_depara_cen-centro_real.
        endif.

      endif.

      " Log geração de carta frete errada
      move: sy-mandt   to wk_0014-mandt,
            wk_vttko-tknum  to wk_0014-tknum,
            wk_vttko-exti1  to wk_0014-conhec,
            wk_vttk-exti2   to wk_0014-ctafrete,
            sy-datum        to wk_0014-data,
            sy-uzeit        to wk_0014-hora,
            sy-uname        to wk_0014-usuario,
            c_n             to wk_0014-reimp,
            c_n             to wk_0014-alterar,
            wk_vttko-exti2  to wk_0014-actafrete,
            e_werks         to wk_0014-werks.

      modify zlest0014 from wk_0014.

      clear: wk_0014.

      " Fecha o registro que permitiu alteração e inserção.
      select *
      into wk_zlest0014
      from zlest0014
      where motivo eq  c_x
      and  ctafrete eq wk_vttk-exti2
      and  alterar  ne c_n.

        move: c_n to wk_zlest0014-alterar,
              c_n to wk_zlest0014-reimp.

        modify zlest0014 from wk_zlest0014.
      endselect.

      " Fecha registos anterior ao inserido
      select *
      into wk_zlest0014
      from zlest0014
      where tknum eq wk_vttk-tknum
      and  conhec eq wk_vttko-exti1
      and  ctafrete ne wk_vttko-exti2.

        move: c_n to wk_zlest0014-alterar,
              c_n to wk_zlest0014-reimp.

        modify zlest0014 from wk_zlest0014.
      endselect.

    endif.

  endif.

  if  ( sy-tcode = 'VT01N' ) and ( p_emite is initial ) and ( wk_vttk-add03 eq '0000000001' ).
    " Busca a Filial e verifica se numero de carta frete já foi cadastrado para inserção.
*    SELECT SINGLE vbeln
*    INTO e_vbeln
*    FROM vttp
*    WHERE tknum = wk_vttk-tknum.

    if <xvttp> is assigned and not
       <xvttp>[] is initial.
      it_vttp[] = <xvttp>[].
      read table it_vttp into wa_vttp index 1.

      select single werks
        into e_werks
        from lips
       where vbeln = wa_vttp-vbeln.

      if wk_vttk-shtyp eq 'Z021'.

        select single * into wa_zsdt_depara_cen
          from zsdt_depara_cen
         where centrov_1 eq e_werks.

        if sy-subrc is initial.
          e_werks = wa_zsdt_depara_cen-centro_real.
        endif.

      endif.

      select single *
        into wk_zlest0014
        from zlest0014
        where ctafrete eq wk_vttk-exti2
        and   werks    eq e_werks.


      if ( sy-subrc is initial ) and  ( wk_zlest0014-motivo  ne c_x ).
        message e067(zles).
        exit.
        "MESSAGE ID cl_msg TYPE 'E' NUMBER nr_msg WITH 'Carta Frete já Cadastrada Anteriormente.' RAISING error_with_message.
      endif.
    endif.
  endif.


* Validações independentes de modal
  check not im_shipments_at_save-new_vttk[] is initial.
* Grupo de contas do transportador
  read table im_shipments_at_save-new_vbpa into wk_vbpa with key parvw = 'SP'
                                                                 posnr = '000000'.

  if not sy-subrc is initial or wk_vbpa-lifnr is initial or wk_vbpa-lifnr = '0000000000'.
    message e014(zles).
    exit.
*    MESSAGE ID cl_msg TYPE 'E' NUMBER nr_msg WITH  'Informar o código do agente de frete (Transportador - SP).'. " RAISING error_with_message.
  endif.

* Dados do fornecimento no transporte
  read table im_shipments_at_save-new_vtrlk into wk_vtrlk index 1.

* Validação frete rodoviário
  if wk_vttk-vsart = '01'.

    "IF ( wk_vttk-shtyp NE 'Z021' ).

    if ( wk_vtrlk-inco1 eq 'CPT' ) and ( ( wk_vttk-sttrg eq '5' ) or ( wk_vttk-sttrg eq '6' ) or ( wk_vttk-sttrg eq '7' ) ).

      if wk_vttk-add03 ne '0000000002'.
        message e000(zles) with 'Incoterms' wk_vtrlk-inco1 'frota deve ser 0000000002!'.
        exit.
        "MESSAGE ID cl_msg TYPE 'E' NUMBER nr_msg WITH  'Incoterms' wk_vtrlk-inco1 'frota deve ser 0000000002!' RAISING error_with_message.
      endif.

      "check if shipment is relevant for freight cost
      if wk_vttk-fbgst is initial.
        message e000(zles) with 'Documento de transporte não relevante para custo!'.
        exit.
        "MESSAGE ID cl_msg TYPE 'E' NUMBER nr_msg WITH  'Documento de transporte não relevante para custo!' RAISING error_with_message.
      endif.

      l_shipment_data-xvttk[] = im_shipments_at_save-new_vttk[].
      l_shipment_data-xvttp[] = im_shipments_at_save-new_vttp[].
      l_shipment_data-xvtts[] = im_shipments_at_save-new_vtts[].
      l_shipment_data-xvtsp[] = im_shipments_at_save-new_vtsp[].
      l_shipment_data-xvbpa[] = im_shipments_at_save-new_vbpa[].
      l_shipment_data-xtrlk[] = im_shipments_at_save-new_vtrlk[].
      l_shipment_data-xtrlp[] = im_shipments_at_save-new_vtrlp[].

      call function 'SD_SCD_COST_INFO_SHIPMENT'
        exporting
          i_shipment_data  = l_shipment_data
          i_opt_batch_mode = true
        importing
          e_result         = e_result.

      if e_result le 0.
        message e000(zles) with 'Não foi gerado estimativa de custo de frete!'.
        exit.
        "MESSAGE ID cl_msg TYPE 'E' NUMBER nr_msg WITH 'Não foi gerado estimativa de custo de frete!' RAISING error_with_message.
      endif.

    endif.

* Validação no numero de remessa por documento de transporte - modal rodoviário
* Controle de remessas atribuídas ao documento de transporte
    if ( wk_vttk-shtyp ne 'Z021' ) and not ( wk_vttk-shtyp eq 'Z001' and wk_vttk-tplst eq '0162' and wk_vttk-add03 eq '0000000002' ).
      clear vl_contrem.
      describe table im_shipments_at_save-new_vttp lines vl_contrem.

      if vl_contrem > 1.
        message e047(zles).
        exit.
        "MESSAGE ID cl_msg TYPE 'E' NUMBER nr_msg WITH 'Não é permitido no modal rodoviário a atribuição de mais de uma remessa' RAISING error_with_message.
      endif.
    endif.

* Tipo de contrato ( Frete peso ou lotação )
    if wk_vttk-sdabw is initial.
      message e017(zles).
      exit.
      "MESSAGE ID cl_msg TYPE 'E' NUMBER nr_msg WITH 'Informar o tipo de contrato ( Frete Peso ou Lotação).' RAISING error_with_message.
    endif.

* Status para o início do carregamento
    if wk_vttk-stlbg = 'X'.

* Validação de próprio
      if wk_vttk-add03 = '0000000001'.

* Validações independentes de modal
* Grupo de contas do transportador
        read table im_shipments_at_save-new_vbpa into wk_vbpa with key parvw = 'PV'
                                                                       posnr = '000000'.

        if not sy-subrc is initial or wk_vbpa-lifnr is initial or wk_vbpa-lifnr = '0000000000'.
          message e007(zles).
          exit.
          "MESSAGE ID cl_msg TYPE 'E' NUMBER nr_msg WITH 'Informar o proprietário do veículo no Guia Parceiros (PV).' RAISING error_with_message.
        endif.

* Validações independentes de modal
* Grupo de contas do transportador
        read table im_shipments_at_save-new_vbpa into wk_vbpa with key parvw = 'MT'
                                                                       posnr = '000000'.

        if not sy-subrc is initial or wk_vbpa-lifnr is initial or wk_vbpa-lifnr = '0000000000'.
          message e010(zles).
          exit.
          "MESSAGE ID cl_msg TYPE 'E' NUMBER nr_msg WITH 'Informar o motorista do veículo no Guia Parceiros (MT).' RAISING error_with_message.
        endif.

      endif.

      if ( wk_vttk-add03 ne '0000000002' ) and ( p_emite is initial ).

*       Número da Carta Frete
        if wk_vttk-exti2 is initial.
          message e019(zles).
          exit.
          "MESSAGE ID cl_msg TYPE 'E' NUMBER nr_msg WITH 'Informar o número da carta frete.' RAISING error_with_message.
        else.

          select single vbeln
          into c_vbeln
          from vttp
          where tknum = wk_vttk-tknum.

          if not sy-subrc is initial.
            read table im_shipments_at_save-new_vttp[] into wa_vttp index 1.
            c_vbeln = wa_vttp-vbeln.
          endif.

          select single werks
            into c_werks
            from lips
           where vbeln = c_vbeln.

          if wk_vttk-shtyp eq 'Z021'.

            select single * into wa_zsdt_depara_cen
              from zsdt_depara_cen
             where centrov_1 eq c_werks.

            if sy-subrc is initial.
              c_werks = wa_zsdt_depara_cen-centro_real.
            endif.

          endif.

          select single *
            into wa_zlest0036
            from zlest0036
            where nr_cf_inicial <= wk_vttk-exti2
              and nr_cf_final   >= wk_vttk-exti2
              and transportadora = wk_vttk-tdlnr
              and branch         = c_werks.
          "AND branch         = wk_vttk-tplst. Comentado conforme chamado 47463

          if not sy-subrc is initial.
            message e000(z01) with 'Numeração da Carta Frete não cadastrada !'.
            exit.
            "MESSAGE ID cl_msg TYPE 'E' NUMBER nr_msg WITH 'Numeração da Carta Frete não cadastrada !' RAISING error_with_message.
          endif.

        endif.

      endif.

*     Status para o fim do transporte
      if wk_vttk-stten = 'X'.

*       Número da conhecimento de embarque
        if wk_vttk-exti1 is initial and
           wk_vttk-add03 ne '0000000002'.
          message e018(zles).
          exit.
          "MESSAGE ID cl_msg TYPE 'E' NUMBER nr_msg WITH 'Informar o número do conhecimento de embarque.!' RAISING error_with_message.
        endif.

      endif.

    endif.


    read table im_shipments_at_save-new_vttp into wk_vttp index 1.

    if sy-subrc is initial.

      select single matnr
      into          wk_matnr
      from          lips
      where         vbeln = wk_vttp-vbeln.

* Determinação da margem de tolerância
      select single *
      into          wk_912
      from          a912
      where         kappl = c_kappl   and
                    kschl = c_kschl   and
                    matnr = wk_matnr  and
                    datab <= sy-datum and
                    datbi >= sy-datum.

      if sy-subrc is initial.

* Valor da condição ZMRG - Margem de tolerância
        select single *
        into          wk_konp
        from          konp
        where         knumh = wk_912-knumh.

        if not sy-subrc is initial.
          message e027(zles) with wk_matnr.
          exit.
          "MESSAGE ID cl_msg TYPE 'E' NUMBER nr_msg WITH wk_matnr 'Não há registro de margem de adto para o material &. Condição (ZMRG/TK11)' RAISING error_with_message.
        endif.

      else.
        message e027(zles) with wk_matnr.
        exit.
        "MESSAGE ID cl_msg TYPE 'E' NUMBER nr_msg WITH wk_matnr 'Não há registro de margem de adto para o material &. Condição (ZMRG/TK11)' RAISING error_with_message.
      endif.

    endif.

* Status de processamento de transporte
    if wk_vttk-stabf = 'X'.

* Validações independentes de modal
* Grupo de contas do transportador
      read table im_shipments_at_save-new_vbpa into wk_vbpa with key parvw = 'PC'
                                                                     posnr = '000000'.

      if not sy-subrc is initial or wk_vbpa-lifnr is initial or wk_vbpa-lifnr = '0000000000'.
        message e015(zles).
        exit.
        "MESSAGE ID cl_msg TYPE 'E' NUMBER nr_msg WITH 'Informar o Ponto de Coleta no Guia Parceiros  (PC).' RAISING error_with_message.
      endif.

* Validações independentes de modal
* Grupo de contas do transportador
      read table im_shipments_at_save-new_vbpa into wk_vbpa with key parvw = 'LR'
                                                                     posnr = '000000'.

      if not sy-subrc is initial or wk_vbpa-kunnr is initial or wk_vbpa-kunnr = '0000000000'.
        message e016(zles).
        exit.
        "MESSAGE ID cl_msg TYPE 'E' NUMBER nr_msg WITH 'Informar o Local de entrega no Guia Parceiros (LR).' RAISING error_with_message.
      endif.

* Validação de remessas / AR atribuídas a mais de um transporte quando Transporte = rodoviário.

      select count( * )
      into vl_cont
      from vttp as a
      inner join vttk as b
      on a~tknum = b~tknum
      for all entries in im_shipments_at_save-new_vttp
      where a~vbeln = im_shipments_at_save-new_vttp-vbeln and
            b~vsart = wk_vttk-vsart                       and
            b~tknum <> wk_vttk-tknum.

      if sy-subrc is initial.
        message e036(zles).
        exit.
        "MESSAGE ID cl_msg TYPE 'E' NUMBER nr_msg WITH 'Remessa atribuida a mais de um doc. de transporte.' RAISING error_with_message.
      endif.

* Validação de nota fiscal
      if wk_vttk-abfer = '1' or wk_vttk-abfer = '3'.

* Desativação temporaria do rotina de validação de nota fiscal 1 = 2.
        if  1 = 2.

          loop at im_shipments_at_save-new_vttp into wk_vttp.

            select single vbelv posnv vbeln posnn vbtyp_n vbtyp_v
            into   vl_vbfa
            from   vbfa
            where  vbelv   = wk_vttp-vbeln   and
                   vbtyp_v = 'J'             and
                   vbtyp_n = 'M'.

            if sy-subrc is initial.

              select count( * )
              into   vl_cont
              from   j_1bnflin as a
              inner join j_1bnfdoc as b
              on a~docnum      = b~docnum
              where  a~refkey  = vl_vbfa-vbeln  and
                     a~refitm  = vl_vbfa-posnn  and
                     a~reftyp  = 'BI'           and
                     b~docstat = '1'            and
                     b~nfe     = 'X'            and
                     b~cancel  <> 'X'.

              if not sy-subrc is initial.

                select count( * )
                into   vl_cont
                from   j_1bnflin as a
                inner join j_1bnfdoc as b
                on a~docnum      = b~docnum
                where  a~refkey  = vl_vbfa-vbeln  and
                       a~refitm  = vl_vbfa-posnn  and
                       a~reftyp  = 'BI'           and
                       b~nfe     <> 'X'           and
                       b~cancel  <> 'X'.

                if not sy-subrc is initial.
                  message e021(zles) with wk_vttp-vbeln.
                  exit.
                  "MESSAGE ID cl_msg TYPE 'E' NUMBER nr_msg WITH wk_vttp-vbeln 'Não há nota fiscal em conformidade para a remessa & .' RAISING error_with_message.

                endif.

              endif.

            else.
              message e020(zles) with wk_vttp-vbeln.
              exit.
              "MESSAGE ID cl_msg TYPE 'E' NUMBER nr_msg WITH wk_vttp-vbeln 'Não há faturamento gerado para a remessa & .' RAISING error_with_message.
            endif.

          endloop.

        endif.

      endif.

    endif.
    "ENDIF.

* Importa controle de calculo de custo de frete Status 5  - Programa ZXV56U16
    import vl_ctrl to vl_ctrl from memory id 'CALCCUSTO'.

    if vl_ctrl = 5 and wk_vttk-stabf = 'X'.
      message e025(zles).
      exit.
      "MESSAGE ID cl_msg TYPE 'E' NUMBER nr_msg WITH 'Impossivel calcular o custo de frete, acionar o departamento de TI.' RAISING error_with_message.
    endif.

    "IF wk_vttk-shtyp NE 'Z021'.

    if p_emite is initial.

      if not wk_vttk-exti2 is initial.
*
*      CLEAR:  vl_cfrete, wk_0014.
*
*      SELECT SINGLE *
*      INTO  wk_0014
*      FROM zlest0014
*      WHERE tknum = wk_vttk-tknum .

** Documento de carta frete.
*      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
*        EXPORTING
*          input  = wk_vttk-exti2
*        IMPORTING
*          output = vl_cfrete.
*
*      MOVE: sy-mandt        TO wk_0014-mandt,
*            wk_vttk-tknum   TO wk_0014-tknum,
*            vl_cfrete       TO wk_0014-ctafrete.
*
*      MODIFY zlest0014 FROM wk_0014.

*      IF wk_vttk-sttbg = 'X'.

*        SELECT SINGLE fknum
*        INTO  vl_fknum
*        FROM  vfkp
*        WHERE rebel  = wk_vttk-tknum AND
*              refty  = '8'.
*
*        IF NOT sy-subrc IS INITIAL.
*          MESSAGE e044(zles).
*        ENDIF.

*      ENDIF.
      else.
        if wk_vttk-add03 ne '0000000002'.
          "MESSAGE e045(zles).
*       BBKO/Pathelle Verifica se não esta sendo chamado pela função ZSD_TRANSP_ETAPA Inicio 09/06/2011
*        MESSAGE ID cl_msg TYPE 'E' NUMBER nr_msg WITH 'Número da carta frete obrigatório' RAISING error_with_message.
          clear vl_etapa.
          import vl_etapa to vl_etapa from memory id 'ZSDETAPAS'.
          free memory id 'ZSDETAPAS'.
          if vl_etapa is initial.
            message e068(zles).
            exit.
            "MESSAGE ID cl_msg TYPE 'E' NUMBER nr_msg WITH 'Número da carta frete obrigatório' RAISING error_with_message.
          endif.
*       BBKO/Pathelle Verifica se não esta sendo chamado pela função ZSD_TRANSP_ETAPA Fim    09/06/2011
        endif.
      endif.
    endif.

  endif.


endmethod.


method IF_EX_BADI_LE_SHIPMENT~BEFORE_UPDATE.
endmethod.


method IF_EX_BADI_LE_SHIPMENT~IN_UPDATE.
endmethod.


method ver_emite_ciot.

  data: p_parid	       type j_1bparid,
        wa_j_1bbranch  type j_1bbranch,
        p_partyp       type j_1bpartyp,
        wa_likp        type likp.

  clear: p_emite.

  if ( wk_vttk-vsart eq '01' ) and ( wk_vttk-add03 eq '0000000001' ) and ( not wa_vtrlp is initial ). "Rodoviário

    wa_j_1bbranch-branch = wa_vtrlp-werks.

    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        input  = wa_j_1bbranch-branch
      importing
        output = wa_j_1bbranch-branch.

    call function 'Z_CENTRO_REAL_VIRTUAL'
      exporting
        centro               = wa_j_1bbranch-branch
      importing
        centro_out           = wa_j_1bbranch-branch
      exceptions
        informar_centro      = 1
        nao_centro_r_virtual = 2
        informar_centro_out  = 3
        informar_centro_v    = 4
        others               = 5.

    if sy-subrc is initial.

      select single * into wa_j_1bbranch
        from j_1bbranch
       where branch eq wa_j_1bbranch-branch.

      if sy-subrc is initial.

        select single * into wa_likp
          from likp
         where vbeln eq wa_vtrlp-vbeln.

        if wa_likp-vbtyp eq '7'.
          p_parid = wa_likp-lifnr.
          p_partyp = 'V'.
        else.
          p_partyp = 'C'.
          call function 'Z_LES_TIPO_REMESSA'
            exporting
              p_vbeln    = wa_vtrlp-vbeln
            changing
              p_parid_rm = p_parid.
        endif.

        call function 'Z_CIOT_EMPRESA_PARCEIRO'
          exporting
            p_empresa    = wa_j_1bbranch-bukrs
            p_partyp     = p_partyp
            p_parid      = p_parid
            p_dt_posicao = wk_vttk-dtdis
            p_route      = wk_vttk-route
          importing
            p_emite      = p_emite.

      endif.

    endif.

  endif.

endmethod.


method ver_mov_intercompany.

  data: wk_vttkvb_tab  type vttkvb.

  move i_new_vttk[] to o_new_vttk[].

  call function 'Z_LES_DETERMINA_FROTA'
    exporting
      wk_vttk = p_vttk
    importing
      add03   = p_add03.

  read table o_new_vttk into wk_vttkvb_tab index 1.

  wk_vttkvb_tab-add03 = p_add03.

  modify o_new_vttk index 1 from wk_vttkvb_tab transporting add03.

endmethod.


method ver_permite_alteracao.

  data: v1_vbeln  type vbak-vbeln,
        v2_vbeln  type vbrp-vbeln,
        v3_docnum type j_1bnflin-docnum,
        v4_cancel type j_1bnfe_active-cancel.

  if ( sy-tcode eq 'VT02N' ) and ( wk_vttk_ant-exti1 ne wk_vttk_atu-exti1 ) and ( wk_vttk_atu-add03 eq '0000000001' ).

    select single vbeln
    from   vbak
    into   v1_vbeln
    where  tknum eq wk_vttk_ant-tknum.
    check sy-subrc is initial.

    select single vbeln
    from   vbrp
    into   v2_vbeln
    where  vgbel eq v1_vbeln AND DRAFT = SPACE .
    check sy-subrc is initial.

    select single docnum
    from   j_1bnflin
    into   v3_docnum
    where  refkey eq v2_vbeln.
    check sy-subrc is initial.

    select single cancel
    from   j_1bnfe_active
    into   v4_cancel
    where  docnum eq v3_docnum.
    check sy-subrc is initial.

    if v4_cancel is initial.
      message e065(zles) raising nao_permite.
    endif.

  endif.

endmethod.
ENDCLASS.

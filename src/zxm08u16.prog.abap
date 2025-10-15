*&---------------------------------------------------------------------*
*&  Include           ZXM08U16
*&---------------------------------------------------------------------*

data: p_lifnr  type lifnr,
      p_nftype type j_1bnftype,
      p_xblnr  type xblnr1,
      p_data   type invdt,
      p_werks  type mseg-werks.

data : wa_bankl        type lfbk-bankl,

       vl_netdt        like e_trbkpv-budat,
       wa_data         type sy-datum,
       w_campo(40),
       v_dados(40),
       t_ekko          type ekko,
       vl_invfo2(40),
       wa_j_1baa       type j_1baa,
       vl_stcd1        type lfa1-stcd1,
       vl_scacd        type lfa1-scacd,
       vl_stkzn        type lfa1-stkzn,
       v1_lifre(40),
       v1_xblnr(40),
       v1_vorgang(40),
       vl_invbldat(40),
       vl_invwerks(40),
       vl_zterm(40),
       vl_zbd1t(40),
       v1_pstyp        type ekpo-pstyp,
       v1_lifnr        type ekko-lifnr,
       v_llief         type ekko-lifnr,
       v_lifre         type ekko-lifnr,
       v_bednr         type ekpo-bednr,
       v2_bsart        type ekko-bsart,
       v1_bsart        type ekko-bsart,
       v1_zterm        type ekko-zterm,
       v1_zbd1t        type ekko-zbd1t,
       v1_bedat        type ekko-bedat,
       wa_setleaf      type setleaf,
       wl_n_uteis      type sy-index,
       vl_ihran        type ekko-ihran,
       "V1_HBKID        TYPE RBKP-HBKID,
       wa_zmmt0066     type zmmt0066,
       wl_sab_dom_fer  type table of iscal_day with header line,
       vdata           type sy-datum.

data: it_texto    type standard table of tline,
      wl_name     type thead-tdname,
      v_wrbtr     type wrbtr,
      value_wrbtr type wrbtr,
      v_number    type p decimals 2.

data: rg_lifnr type range of lifnr,
      rg_gsber type range of gsber,
      rg_mwskz type range of mwskz.


data: t_zib_nfe_forn type table of zib_nfe_forn.
data: t_set type standard table of setleaf  with header line.
data: t_lay type standard table of setlinet with header line.

data: w_rbws     like line of e_trbkpv-h_rbws,
      xachou(1),
      xachou2(1),
      xcontli    type i,
      xcont99    type i.

field-symbols: <fs_vorgang2> type any.

if ( sy-ucomm eq 'OK' and sy-tcode eq 'MIR4' and sy-dynnr = '6108' ).
  message e398(00) with 'Utilize a MR8M para estorno!'.
endif.

" Regra 06/08/2015 a pedido de Marcos Santos (ALRS)
if ( sy-tcode eq 'MIRO' ) or ( sy-tcode eq 'MIR4'). " Adicionado tipo MIR4. Usuário Memoriza e depois registra documento.
  w_campo = '(SAPLMR1M)RM08M-VORGANG'.
  assign (w_campo) to <fs_vorgang2>.
  loop at e_tdrseg .
    select single bsart
       from ekko
        into v1_bsart
       where  ebeln eq e_tdrseg-ebeln.
    if sy-subrc = 0.
      if v1_bsart = 'ZGR'.
        select single *
          from setleaf
          into wa_setleaf
          where setname eq 'MAGGI_PED_GRAOS'
           and valfrom eq sy-uname.
        if sy-subrc ne 0.
          message e398(00) with 'Para o tipo de Pedido' v1_bsart
                          'não pode ser lançado MIRO direto '
                          'somente via SIGAM'.
        endif.
      elseif ( 'PCE_PCEI_PCS_PCSI_PCEF_PSEF' cs v1_bsart ). "  AND  <FS_VORGANG2> = 1 .

        select single * from zmmt0110 into @data(wa_zmmt0110)
          where bukrs eq @e_trbkpv-bukrs
           and  usnam eq @sy-uname.

        if e_trbkpv-budat ne sy-datum and not ( '0101_0100'  cs e_trbkpv-bukrs ) and wa_zmmt0110-usnam is initial.
          message e398(00) with 'Data de Lançamento deve ser a Atual!'.
        endif.
      endif.
    endif.
  endloop.

  "CS2016000936
  "Controle data de lançamento Fiscal
  data: vg_last_day  type sy-datum,
        vg_first_day type sy-datum.

*  CONCATENATE SY-DATUM(6) '01' INTO VG_FIRST_DAY.
*  CALL FUNCTION 'BKK_GET_MONTH_LASTDAY'
*    EXPORTING
*      I_DATE = VG_FIRST_DAY
*    IMPORTING
*      E_DATE = VG_LAST_DAY.
*
*  REFRESH WL_SAB_DOM_FER.
*  CALL FUNCTION 'HOLIDAY_GET'
*    EXPORTING
*      FACTORY_CALENDAR           = 'ZF'
*      DATE_FROM                  = SY-DATUM
*      DATE_TO                    = VG_LAST_DAY
*    TABLES
*      HOLIDAYS                   = WL_SAB_DOM_FER
*    EXCEPTIONS
*      FACTORY_CALENDAR_NOT_FOUND = 1
*      HOLIDAY_CALENDAR_NOT_FOUND = 2
*      DATE_HAS_INVALID_FORMAT    = 3
*      DATE_INCONSISTENCY         = 4
*      OTHERS                     = 5.
*
*  READ TABLE WL_SAB_DOM_FER WITH KEY DATE = SY-DATUM.
*  IF SY-SUBRC = 0.
*    MESSAGE E398(00) WITH 'Lançamento não é dia útil'.
*  ELSE.
*    "Verifica ultimo dia útil
*    DO.
*      READ TABLE WL_SAB_DOM_FER WITH KEY DATE = VG_LAST_DAY.
*      IF SY-SUBRC NE 0.
*        EXIT.
*      ENDIF.
*      SUBTRACT 1 FROM VG_LAST_DAY.
*    ENDDO.
*    IF E_TRBKPV-BUDAT EQ VG_LAST_DAY.
*      SELECT SINGLE *
*        FROM ZMMT0066
*        INTO WA_ZMMT0066
*        WHERE USNAM = SY-UNAME.
*      IF SY-SUBRC NE 0.
*        MESSAGE E398(00) WITH 'Lançamento é ultimo dia útil, procurar Depto fiscal'.
*      ENDIF.
*    ENDIF.
*  ENDIF.

  "Informar conta bancária no ped. compras #128658 - BG
  select single bvtyp
        from zmmt0035
         into @data(v1_bvtyp)
        where  ebeln eq @e_tdrseg-ebeln.


  if v1_bvtyp is not initial.
    if v1_bvtyp ne e_trbkpv-bvtyp. "Informar conta bancária no ped. compras #138397 - RPS
      message e398(00) with 'Banco parceiro diferente do informado na solicitação de pedido'.
    endif.
  endif.

endif.

if ( e_trbkpv-bukrs = '0100' ) and  sy-tcode eq 'MIRO'. " Inicio Validação Empresa 0100 Argentina Imposot retido

  select single bsart
      from ekko
       into v1_bsart
      where  ebeln eq e_tdrseg-ebeln.

  if v1_bsart = 'PCE'.
    clear: xcontli, xcont99, xachou2.
    loop at e_trbkpv-h_rbws into w_rbws.

      if w_rbws-witht+0(1) = 'G'.
        continue.
      endif.

      clear xachou.
      loop at e_tdrseg.
        add 1 to xcontli.
        clear v_bednr.
        select single bednr
          into v_bednr
          from ekpo
          where ebeln = e_tdrseg-ebeln
          and   ebelp = e_tdrseg-ebelp.

        if v_bednr is not initial and v_bednr ne '99'.
          refresh: t_set, t_lay.
          select *
           from  setleaf
           into table t_set
           where setclass      = '0000'
           and   setname       = 'MAGGI_0100_RETEN'.

          delete t_set where valfrom+0(2) <>  v_bednr+0(2).
          if t_set[] is not initial.
            select *
              from setlinet
              into table t_lay
              for all entries in t_set
              where setclass   = t_set-setclass
              and subclass     = t_set-subclass
              and setname      = t_set-setname
              and langu        = 'P'
              and lineid       = t_set-lineid.
          endif.

          loop at t_lay.
            if t_lay-descript cs w_rbws-witht.
              xachou = 'X'.
              exit.
            endif.
          endloop.
        elseif v_bednr eq '99'.
          add 1 to xcont99.
        endif.

      endloop.
      if xachou  is initial and w_rbws-wt_qsshb gt 0.
        message e398(00) with 'Impuesto'
                              w_rbws-witht
                              ' invalido para provincia'.
      elseif w_rbws-wt_qsshb gt 0.
        xachou2 = 'X'.
      endif.

    endloop.
    if  xcont99 =  xcontli.
      xachou2 = 'X'.
    endif.
    if xachou2  is initial.
      message e398(00) with 'Ninguno Impuesto'
                               ' encontrado'
                               t_lay-descript.
    endif.
  endif.
endif.



"Inicio USER STORY 76420 =====================================Anderson Oenning
select single bsart
from ekko
into @data(v_bsart)
where  ebeln eq @e_tdrseg-ebeln.

if e_trbkpv-bukrs eq '0100' and v_bsart ne 'ZGF' and  v_bsart ne 'ZFTE'. "Compara valor total do pedido com o valor total da fatura da MIRO.

  field-symbols: <fs_doc>  type any,
                 <fs_info> type invfo.


  w_campo = '(SAPLMR1M)RM08M-VORGANG'.
  assign (w_campo) to <fs_doc>.

  if <fs_doc> eq '1'. "Fatura
*
    wl_name = e_tdrseg-ebeln.
    call function 'READ_TEXT'
      exporting
        id                      = 'F03'
        language                = 'S'
        name                    = wl_name
        object                  = 'EKKO'
      tables
        lines                   = it_texto
      exceptions
        id                      = 1
        language                = 2
        name                    = 3
        not_found               = 4
        object                  = 5
        reference_check         = 6
        wrong_access_to_archive = 7
        others                  = 8.

    if it_texto is not initial.
      read table it_texto into data(lwa_texto) index 1.
      if lwa_texto is not initial.
        "Convert char em valor.
        call function 'MOVE_CHAR_TO_NUM'
          exporting
            chr             = lwa_texto-tdline
          importing
            num             = v_number
          exceptions
            convt_no_number = 1
            convt_overflow  = 2
            others          = 3.

        if sy-subrc eq 0.

          "Comparar o valor total do pedido com o valor total da fatura da MIRO.
          move v_number to value_wrbtr. "Valor total do pedido.


          v_dados = '(SAPLFDCB)INVFO'.
          assign (v_dados) to <fs_info>. "Valor total da MIRO
          if <fs_info>-wrbtr ne value_wrbtr.
            message e398(00) with 'EL IMPORTE TOTAL '
                                   'DE LA FACTURA ES '
                                   'DIFERENTE DEL '
                                   'IMPORTE TOTAL DEL PEDIDO!'.
          endif.
        endif.
      endif.
    endif.
  endif.
endif.
clear: v_bsart.
"Fim USER STORY 76420 =====================================Anderson Oenning

if not ( '0101_0100'  cs e_trbkpv-bukrs ). " Inicio Validação Empresa 0100 Argentina
  refresh wl_sab_dom_fer.
  clear: wa_bankl.
  "break abap.
  field-symbols: <zuonr> type any.

  field-symbols: <fs_invfo>   type invfo,
                 <fs_vorgang> type any.

  if sy-tcode eq 'MIRO'.
    w_campo = '(SAPLFDCB)INVFO'.
    assign (w_campo) to <fs_invfo>.

    "Tem Relevância Fiscal
    if <fs_invfo>-j_1bnftype is not initial.

      select * into table @data(it_zmmt0128)
        from zmmt0128
       where j_1bnftype eq @<fs_invfo>-j_1bnftype.

      "Se existe configuração para a categoria, deve ser respeitado
      if sy-subrc is initial.
        data: wa_e_tdrseg type mmcr_drseg.
        loop at e_tdrseg into wa_e_tdrseg .
          read table it_zmmt0128 with key mwskz_bnk = wa_e_tdrseg-mwskz transporting no fields.
          if sy-subrc is not initial.
            message e118(zfi) with <fs_invfo>-j_1bnftype wa_e_tdrseg-mwskz.
          endif.
        endloop.
      else.
        message e121(zfi) with <fs_invfo>-j_1bnftype.
      endif.
    endif.

  endif.

  if ( sy-tcode eq 'MIRO' ) or ( sy-tcode eq 'MIR4'). " Adicionado tipo MIR4. Usuário Memoriza e depois registra documento.
    " Regra chamado 52577
    read table e_tdrseg index 1.

    select single werks
     from ekpo
      into @data(v_werks)
     where  ebeln eq @e_tdrseg-ebeln.

    w_campo = '(SAPLFDCB)INVFO'.
    assign (w_campo) to <fs_invfo>.

    if <fs_invfo>-gsber ne e_tdrseg-gsber.
      message e398(00) with 'Divisão incorreta '
                            <fs_invfo>-gsber.
    endif.

    "wa_data = <fs_invfo>-zfbdt.
    wa_data = sy-datum.

    w_campo = '(SAPLMR1M)RM08M-VORGANG'.
    assign (w_campo) to <fs_vorgang>.

*    IF <FS_VORGANG> = '2'. "nota de crédito
*      IF <FS_INVFO>-BLDAT NE SY-DATUM.
*        MESSAGE E398(00) WITH 'Data de documento para NOTA DE CREDITO'
*                         ' deve se a ATUAL'.
*      ENDIF.
*
*      IF <FS_INVFO>-BUDAT NE SY-DATUM.
*        MESSAGE E398(00) WITH 'Data de lançamento para NOTA DE CREDITO'
*                         ' deve se a ATUAL'.
*      ENDIF.
*
*
*    ENDIF.

    add 3 to wa_data.
    call function 'HOLIDAY_GET'
      exporting
*       HOLIDAY_CALENDAR           = ' '
        factory_calendar           = 'ZF'
        date_from                  = sy-datum
        date_to                    = <fs_invfo>-netdt
      tables
        holidays                   = wl_sab_dom_fer
      exceptions
        factory_calendar_not_found = 1
        holiday_calendar_not_found = 2
        date_has_invalid_format    = 3
        date_inconsistency         = 4
        others                     = 5.


    describe table wl_sab_dom_fer lines wl_n_uteis.

    add wl_n_uteis to wa_data.

    if ( wa_data gt <fs_invfo>-netdt ) and ( <fs_invfo>-zlspr ne 'Z' ) and
       ( ( <fs_vorgang> ne '2' ) and ( <fs_vorgang> ne '4' )  ).
      message e398(00) with 'Fatura fora do prazo de pgto 72 horas, alterar'
                            ' vencimento ou será bloqueada e somente poderá'
                            ' ser liberada pelo Dpto.Liquidação ramal 3053.'
                            ' Bloqueio pgto deve ser Z-Fora prazo pgto 72hr!'.
    elseif ( <fs_invfo>-zlspr ne 'Z' ) and
           ( ( <fs_vorgang> ne '2' ) and ( <fs_vorgang> ne '4' )  ).
      read table wl_sab_dom_fer with key date = <fs_invfo>-netdt.
      if sy-subrc = 0.
        message e398(00) with 'Vencimento não é dia útil'.
      endif.

    endif.

    "Início - DEVK9A1KSM - 03.10.2023 FI - CS2023000239 bloqueia MIRO entrega futura #108062 RSA
    if <fs_invfo>-zlspr ne 'X'.
*      IF ( 'PSEF-YCEF-YSEF-ZEFI-ZGEF-ZEF' CS v1_bsart ).
      if v1_bsart = 'PSEF' or
         v1_bsart = 'PCEF' or
         v1_bsart = 'YCEF' or
         v1_bsart = 'YSEF' or
         v1_bsart = 'ZEFI' or
         v1_bsart = 'ZGEF' or
         v1_bsart = 'ZEF'.
        message e398(00) with 'Pedidos entrega futura, Campo Bloq. = MIRO Fatura'.
      endif.
    endif.
    "FIm - DEVK9A1KSM - 03.10.2023 FI - CS2023000239 bloqueia MIRO entrega futura #108062 RSA


    " Regra chamado 52577
    read table e_tdrseg index 1.

    select single bsart
     from ekko
      into v1_bsart
     where  ebeln eq e_tdrseg-ebeln.

    "CS2016001792
    if   ( ( v1_bsart eq 'ZDEF' ) or ( v1_bsart eq 'ZSEM' ) or ( v1_bsart eq 'ZFTE' ) ).
      if <fs_invfo>-blart ne 'IN'.
        message e398(00) with 'Para o tipo de Pedido' v1_bsart
                          ' é obrigado Tp.Doc'
                          ' igual a IN-Fatura brut.INSUMOS'.
      endif.
    endif.
    "CS2016001494
*    IF  ( ( ( V1_BSART EQ 'ZDEF' ) OR ( V1_BSART EQ 'ZSEM' ) OR ( V1_BSART EQ 'ZFTE' ) )  AND ( <FS_INVFO>-ZLSPR NE 'I' ) ) .
    if  ( ( ( v1_bsart eq 'ZSEM' ) or ( v1_bsart eq 'ZFTE' ) )  and ( <fs_invfo>-zlspr ne 'I' ) ) .
      message e398(00) with 'Para o tipo de Pedido' v1_bsart
                            'é obrigado a bloquear a fatura'
                            ' com tipo igual a I-Compras de Insumos.'.

    endif.

    field-symbols: <fs_invfo2>    type any,
                   <fs_lifre>     type any,
                   <fs_xblnr>     type any,
                   <fs_vorgang_v> type any,
                   <fs_data_fat>  type any,
                   <fs_werks>     type any,
                   <fs_zterm>     type any,
                   <fs_zbd1t>     type any.

    vl_invfo2 = '(SAPLFDCB)INVFO-J_1BNFTYPE'.
    assign (vl_invfo2) to <fs_invfo2>.

    vl_invbldat = '(SAPLFDCB)INVFO-BLDAT'.
    assign (vl_invbldat) to <fs_data_fat>.

    v1_lifre = '(SAPLFDCB)INVFO-LIFRE'.
    assign (v1_lifre) to <fs_lifre>.

    v1_xblnr = '(SAPLFDCB)INVFO-XBLNR'.
    assign (v1_xblnr) to <fs_xblnr>.

    v1_vorgang = '(SAPLMR1M)RM08M-VORGANG'.
    assign (v1_vorgang) to <fs_vorgang_v>.


    vl_invwerks = '(SAPLMR1M)DRSEG-WERKS'.
    assign (vl_invwerks) to <fs_werks>.

    vl_zterm = '(SAPLMR1M)INVFO-ZTERM'.
    assign (vl_zterm) to <fs_zterm>.

    vl_zbd1t = '(SAPLMR1M)INVFO-ZBD1T'.
    assign (vl_zbd1t) to <fs_zbd1t>.

    move: <fs_lifre>    to p_lifnr,
          <fs_invfo2>   to p_nftype,
          <fs_xblnr>    to p_xblnr,
          <fs_data_fat> to p_data,
          <fs_werks>    to p_werks.

    select single *
      from setleaf
      into wa_setleaf
     where setname eq 'MAGGI_EMPRESA_EXTERIOR'
       and valfrom eq e_trbkpv-bukrs.

    if ( sy-subrc ne 0 ).
      call function 'Z_SD_VERIFICA_FORN_DOC_FISCAL'
        exporting
          p_lifnr       = p_lifnr
          p_nftype      = p_nftype
          p_xblnr       = p_xblnr
          p_data        = p_data
          p_werks       = p_werks
          p_ret_inf_xml = 'X'
        tables
          t_nfe_forn    = t_zib_nfe_forn
        exceptions
          error         = 1
          others        = 2.

      if not sy-subrc is initial.
        message id sy-msgid type sy-msgty number sy-msgno with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      endif.
    else.
      exit.
    endif.
    read table e_tdrseg index 1.

    "IR147486 ALRS
    "CS2016000936 ALRS 11/11/16 ALRS
    select single bsart bedat lifnr zterm zbd1t llief lifre ihran
     from ekko
      into (v1_bsart,v1_bedat,v1_lifnr,v1_zterm , v1_zbd1t, v_llief,v_lifre, vl_ihran)
     where  ebeln eq e_tdrseg-ebeln.

    "Checar todos os pedidos
    if p_lifnr ne  v1_lifnr.
      if ( p_lifnr eq  v_llief ).
      else.
        if  p_lifnr eq  v_lifre.
        else.
          message e398(00) with 'Fornecedor diferente do pedido.'.
        endif.
      endif.
    endif.
*---> CS1067543 / IR127939 --->
*    IF   'PCS_PCSI_PSEF_YCS_YCSI_YSEF' CS v1_bsart AND  <fs_vorgang> = 1.

    if <fs_vorgang> = 1.

      v2_bsart = v1_bsart.

      select single valfrom into v2_bsart
        from setleaf
         where setname = 'PEDIDO_MIRO'
          and valfrom eq v2_bsart.

      if sy-subrc eq 0.
        "US171299
        select single * from zmmt0112 into @data(wa_zmmt0112)
           where ebeln eq @e_tdrseg-ebeln.
        "US171299
        if sy-subrc ne 0.
          if v2_bsart+0(1) = 'Y'.
            v1_bedat = vl_ihran.
          endif.

          if p_data lt v1_bedat.
            message e398(00) with 'Data da Fatura menor que data do Pedido.' v1_bsart.
          endif.
        endif.

*CS2022000178 - Regra de bloqueio data base pedidos PCS     |
*      IF  p_data NE <fs_invfo>-zfbdt.
*        MESSAGE e398(00) WITH 'Data básica deve ser igual a data da fatura!.' v1_bsart.
*      ENDIF.
*CS2022000178 - Regra de bloqueio data base pedidos PCS     |

*      IF P_LIFNR NE  V1_LIFNR.
*        IF ( P_LIFNR EQ  V_LLIEF ).
*        ELSE.
*          IF  P_LIFNR EQ  V_LIFRE.
*          ELSE.
*            MESSAGE E398(00) WITH 'Fornecedor diferente do pedido.'.
*          ENDIF.
*        ENDIF.
*      ENDIF.

        if ( ( v1_zterm is not initial ) and ( <fs_invfo>-zterm ne v1_zterm ) ) or
           ( ( v1_zbd1t is not initial ) and ( <fs_invfo>-zbd1t ne v1_zbd1t ) ). "´permite aumentar os dias para sair do dia nao util
          wl_n_uteis = <fs_invfo>-zbd1t - v1_zbd1t.
          if  wl_n_uteis lt 0 or ( ( v1_zterm is not initial ) and ( <fs_invfo>-zterm ne v1_zterm ) ) .
            message e398(00) with 'Somente permitido prorrogar  '
                                    'a data de vencimento Pedido. Procurar '
                                    'departamento de Suprimentos!'.
          else. "proximo dia util
*          DO 5 TIMES.
*            ADD 1 TO V1_ZBD1T.
*            VL_NETDT = E_TRBKPV-ZFBDT + V1_ZBD1T.
*            REFRESH WL_SAB_DOM_FER .
*            CALL FUNCTION 'HOLIDAY_GET'
*              EXPORTING
**               HOLIDAY_CALENDAR           = ' '
*                FACTORY_CALENDAR           = 'ZF'
*                DATE_FROM                  = VL_NETDT
*                DATE_TO                    = VL_NETDT
*              TABLES
*                HOLIDAYS                   = WL_SAB_DOM_FER
*              EXCEPTIONS
*                FACTORY_CALENDAR_NOT_FOUND = 1
*                HOLIDAY_CALENDAR_NOT_FOUND = 2
*                DATE_HAS_INVALID_FORMAT    = 3
*                DATE_INCONSISTENCY         = 4
*                OTHERS                     = 5.
*
*            DESCRIBE TABLE WL_SAB_DOM_FER LINES WL_N_UTEIS.
*            IF  WL_N_UTEIS = 0.
*              EXIT.
*            ENDIF.
*          ENDDO.
*          IF VL_NETDT NE <FS_INVFO>-NETDT.
*            MESSAGE E398(00) WITH 'As condições de pagamento estão '
*                                  'divergentes do Pedido. Procurar '
*                                  'departamento de Suprimentos!'.
*          ENDIF.
          endif.
        endif.
      endif.
    endif.

    select single pstyp
     from ekpo
      into v1_pstyp
     where ebeln eq e_tdrseg-ebeln
       and ebelp  eq e_tdrseg-ebelp.

    " Fim Regra para validação de XML chamado 52576.
    clear: vl_invfo2,
           v1_lifre,
           v1_xblnr,
           v1_vorgang,
           wa_j_1baa,
           vl_stcd1,
           vl_scacd,
           vl_stkzn,
           v1_pstyp.

  endif.

  w_campo(40) = '(SAPLMRMP)RBKPV-ZUONR'.
  assign (w_campo) to <zuonr>.
  read table e_tdrseg index 1.
  <zuonr> = e_tdrseg-ebeln.

*Início modificação Rollout 03.02.2008.
  check sy-tcode <> 'MRRL'.
  check sy-tcode <> 'ZLES0043'.
*Fim modificação Rollout 03.02.2008.

  "ALRS 16.04.2015 - MSANTOS
  if e_trbkpv-bukrs ne '0101'.
    if e_trbkpv-zlsch = 'P'.
      message e011(zfi).
    elseif e_trbkpv-zlsch = space.
      message e012(zfi).
    endif.
  endif.

*  SELECT SINGLE BANKL
*    FROM LFBK
*    INTO WA_BANKL
*    WHERE LIFNR = E_TRBKPV-LIFNR.
*
*  IF SY-SUBRC <> 0.
*
*    IF E_TRBKPV-ZLSCH NE 'C' AND
*       E_TRBKPV-ZLSCH NE 'E' AND
*       E_TRBKPV-ZLSCH NE 'D'.
*      MESSAGE E015(ZFI).
*    ENDIF.
*    IF E_TRBKPV-BUKRS EQ '0005'.
*      IF E_TRBKPV-HBKID NE 'BBRA'.
*        MESSAGE E014(ZFI).
*      ENDIF.
*
*    ELSE.
*      IF ( E_TRBKPV-HBKID NE 'BBRA' ) AND NOT ( E_TRBKPV-HBKID EQ 'BBD' AND E_TRBKPV-ZLSCH EQ 'D' ).
*        MESSAGE E014(ZFI).
*      ENDIF.
*    ENDIF.
*
*    ASSIGN (W_CAMPO) TO <ZUONR>.
*    READ TABLE E_TDRSEG INDEX 1.
*    <ZUONR> = E_TDRSEG-EBELN.
*
*    EXIT.
*
*  ELSE.
*    IF ( ( E_TRBKPV-ZLSCH NE 'E' ) AND ( E_TRBKPV-ZLSCH NE 'C' ) AND ( E_TRBKPV-ZLSCH NE 'D' ) ).
*      IF E_TRBKPV-BVTYP IS INITIAL.
*        MESSAGE E016(ZFI).
*      ENDIF.
*    ENDIF.
*
*  ENDIF.

*  SELECT SINGLE BANKL
*    FROM LFBK
*    INTO WA_BANKL
*    WHERE LIFNR = E_TRBKPV-LIFNR AND
*          BVTYP = E_TRBKPV-BVTYP.
*
*  IF NOT WA_BANKL IS INITIAL AND ( ( SY-TCODE EQ 'MIRO' ) OR ( SY-TCODE EQ 'MIR4') ).
*
*    SELECT *
*      FROM  SETLEAF
*      INTO TABLE T_SET
*      WHERE SETCLASS      = '0000'
*      AND   SETNAME        = 'MAGGI_BANCO_EMPR'.
*
*    SELECT *
*      FROM SETLINET
*      INTO TABLE T_LAY
*      FOR ALL ENTRIES IN T_SET
*      WHERE SETCLASS   = T_SET-SETCLASS
*      AND SUBCLASS     = T_SET-SUBCLASS
*      AND SETNAME      = T_SET-SETNAME
*      AND LANGU        = 'P'
*      AND LINEID       = T_SET-LINEID.
*
*    READ TABLE T_SET WITH KEY VALFROM = E_TRBKPV-BUKRS.
*    IF SY-SUBRC = 0.
*      READ TABLE T_LAY WITH KEY  SETCLASS   = T_SET-SETCLASS
*                                 SUBCLASS   = T_SET-SUBCLASS
*                                 SETNAME    = T_SET-SETNAME
*                                 LINEID     = T_SET-LINEID.
*      IF SY-SUBRC = 0.
*        V1_HBKID = T_LAY-DESCRIPT+0(5).
*        IF ( E_TRBKPV-HBKID NE V1_HBKID  ).
*          MESSAGE E014(ZFI).
*        ENDIF.
*      ELSE.
*        MESSAGE E014(ZFI).
*      ENDIF.
*    ELSE. "somente BBRA para todas as situações
*      IF ( E_TRBKPV-HBKID NE 'BBRA' ).
*        IF  E_TRBKPV-HBKID EQ 'BBD' AND E_TRBKPV-ZLSCH EQ 'D'. "exceção para boleto de consumo
*        ELSE.
*          MESSAGE E014(ZFI).
*        ENDIF.
*      ENDIF.
*    ENDIF.
*
*    IF WA_BANKL(3) = '001'.
*      V1_HBKID = 'BBRA'.
*    ELSEIF WA_BANKL(3) = '237'.
*      V1_HBKID = 'BBD'.
*    ELSEIF WA_BANKL(3) = '399'.
*      V1_HBKID = 'HSBC'.
*    ENDIF.
*
*
*    IF NOT E_TRBKPV-ZLSCH IS INITIAL.
*
*      IF  E_TRBKPV-HBKID EQ 'BBD' AND E_TRBKPV-ZLSCH EQ 'D'. "exceção para boleto de consumo
*
*      ELSE.
*        IF V1_HBKID <> E_TRBKPV-HBKID AND  E_TRBKPV-ZLSCH EQ 'U'.
*          MESSAGE E015(ZFI).
*        ENDIF.
*
*        IF V1_HBKID = E_TRBKPV-HBKID AND  E_TRBKPV-ZLSCH EQ 'S'.
*          MESSAGE E015(ZFI).
*        ENDIF.
*
*
*        IF V1_HBKID = E_TRBKPV-HBKID AND  E_TRBKPV-ZLSCH NE 'U'.
*          IF E_TRBKPV-ZLSCH NE 'E'.
*            MESSAGE E015(ZFI).
*          ENDIF.
*        ENDIF.
*
*        "'E' BBRA
*        IF E_TRBKPV-HBKID <> 'BBRA' AND  E_TRBKPV-ZLSCH EQ 'E'.
*          MESSAGE E015(ZFI).
*        ENDIF.
*
*        "'D' BBD somente
*        IF E_TRBKPV-HBKID <> 'BBD' AND  E_TRBKPV-ZLSCH EQ 'D'.
*          MESSAGE E015(ZFI).
*        ENDIF.
*
*        "Somente BBRA aceita 'U' IR85492
*        IF E_TRBKPV-HBKID NE 'BBRA' AND E_TRBKPV-ZLSCH EQ 'U' .
*          MESSAGE E015(ZFI).
*        ENDIF.
*      ENDIF.
*
*    ENDIF.
*
*  ENDIF.

*  Validar datas
  if not ( e_trbkpv-bldat le e_trbkpv-budat ).
    "MESSAGE E017(ZFI).
    message 'Data da Fatura não pode ser maior que a data do lançamento!' type 'E'.
  endif.

  vl_netdt = e_trbkpv-zfbdt + e_trbkpv-zbd1t.
* Modificação - Eduardo Ruttkowski Tavares

  field-symbols: <fs_stgrd> type any.

  constants c_campo(17) value '(SAPLMRMP)H_STGRD'.

  assign (c_campo) to <fs_stgrd>.

  if sy-tcode ne 'MR8M'.
    if sy-subrc eq 0.
      if not ( vl_netdt ge e_trbkpv-budat ) and
        <fs_stgrd> ne 'Z1'.
        message e018(zfi).
      endif.
    else.
      if not ( vl_netdt ge e_trbkpv-budat ).
        message e018(zfi).
      endif.
    endif.
  endif.
endif. " Termina Validação da empresa 0100


"Inicio validação CS2022000972 Bloqueio lançamento NF sem categoria / Anderson Oenning.
"Verifica se a categoria da nota esta preenchida.
if ( sy-tcode eq 'MIRO' ) or ( sy-tcode eq 'MIR4').

  "Validação apenas para empresa no Brasil - BR
  select single * from t001 into @data(ws_t001) where bukrs eq @e_trbkpv-bukrs and land1 eq 'BR'.
  if sy-subrc eq 0.

    field-symbols: <j_1bnftype> type any.
    clear: w_campo.

    v_dados = '(SAPLFDCB)INVFO'.
    assign (v_dados) to <fs_info>.

    if <fs_info>-j_1bnftype is initial.

      if <fs_info>-lifnr is not initial.
        rg_lifnr = value #( ( sign = 'I' option = 'EQ'  low = <fs_info>-lifnr ) ).
      endif.

      if <fs_info>-mwskz is not initial.
        rg_mwskz = value #( ( sign = 'I' option = 'EQ'  low = <fs_info>-mwskz ) ).
      endif.

**MM - Bloqueio lançamento NF sem categoria #93901 AO
*      IF e_tdrseg-gsber IS NOT INITIAL.
*        rg_gsber  = VALUE #( ( sign = 'I' option = 'EQ'  low = e_tdrseg-gsber ) ).
*      ENDIF.
**MM - Bloqueio lançamento NF sem categoria #93901 AO

**&-----------------------------------------BUG SOLTO 105293 / Retirar o bloqueio como obrigatorio campo FILIAL, FORNECEDOR transação ZMM0206 / AOENNING. ( Retirado a validação desse chamado 105293.
      select single * from zmmt0164 into @data(ws_zmmt0164) where bukrs eq @e_trbkpv-bukrs and werks in @rg_gsber and lifnr in @rg_lifnr and mwskz in @rg_mwskz.
      if sy-subrc ne 0.
        message e352(8b).
      endif.
    endif.
    clear: ws_zmmt0164.
  endif.
endif.
"Fim validação CS2022000972 Bloqueio lançamento NF sem categoria.


*** PBI - 60951 - Inicio.
if ( sy-tcode eq 'MIRO' ) or ( sy-tcode eq 'MIR4'). " Adicionado tipo MIR4. Usuário Memoriza e depois registra documento.

  field-symbols: <kidno> type any.
  data: lva_nro_sol_cp type zmmt0037-nro_sol_cp.

  read table e_tdrseg index 1.

  select single nro_sol_cp
   from zmmt0037
    into lva_nro_sol_cp
   where  ebelp eq e_tdrseg-ebeln.

  if lva_nro_sol_cp is initial.
    select single nro_sol_cp
      from zmmt0035
      into lva_nro_sol_cp
    where ebeln eq e_tdrseg-ebeln.
  endif.

  w_campo(40) = '(SAPLFDCB)INVFO-KIDNO'.
  assign (w_campo) to <kidno>.

  <kidno>  = lva_nro_sol_cp.

endif.

*** PBI - 60951 - Fim.

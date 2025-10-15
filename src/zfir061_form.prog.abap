*&---------------------------------------------------------------------*
*&  Include           ZFIR060_FORM
*&---------------------------------------------------------------------*


form F_SELECIONAR_DADOS .

  data TABIX type SY-TABIX.

  V_BUKRS = P_BUKRS.

  select single *
     from ZFIT0130
     into WA_ZFIT0130
     where BUKRS  = P_BUKRS
     and   GJAHR  = P_AUGDT-LOW+0(4)
     and   REGIME = 'CX'
     and   STATUS = ''.

  select *
    from ZFIT0131
    into table IT_ZFIT0131
    where ID_SISTEMA = ''
    or    ID_SISTEMA = '0'. "Somente SAP

  if IT_ZFIT0131[] is initial.
    message 'Não existem contas patrimoniais SAP cadastradas!' type 'W'.
    exit.
  endif.

  select SAKNR MITKZ "#EC CI_DB_OPERATION_OK[2431747]
    from SKB1
    into table IT_SKB1
    for all entries in IT_ZFIT0131
    where BUKRS eq P_BUKRS
    and   SAKNR eq IT_ZFIT0131-HKONT
    and   SAKNR in P_HKONT.

  if IT_SKB1[] is not initial.
    select SAKNR TXT50
     from SKAT
     into table IT_SKAT
     for all entries in IT_SKB1
     where SAKNR eq IT_SKB1-SAKNR
     and   SPRAS eq SY-LANGU
     and   KTOPL eq '0050'.

    " Cliente
    select  BSAD~BUKRS BSAD~AUGDT BSAD~HKONT BSAD~AUGBL BSAD~AUGGJ
           BSAD~BELNR BSAD~GJAHR BSAD~BUZEI BSAD~BUDAT BSAD~WAERS BSAD~GSBER BSAD~WRBTR
           BSAD~DMBTR BSAD~DMBE2 BSAD~SGTXT BSAD~SHKZG BKPF~STBLG BKPF~STJAH
     from BSAD
     inner join BKPF
     on  BKPF~BUKRS = BSAD~BUKRS
     and BKPF~BELNR = BSAD~BELNR
     and BKPF~GJAHR = BSAD~GJAHR
     into table IT_BSAD
     for all entries in IT_SKB1
     where BSAD~BUKRS eq  P_BUKRS
     and   BSAD~AUGDT in P_AUGDT
     and   ( BSAD~AUGBL in P_BELNR or BSAD~BELNR in P_BELNR )
     and   BSAD~HKONT eq IT_SKB1-SAKNR
     and   BSAD~WAERS ne 'BRL'
     and   BSAD~BLART ne 'VC'
     and   BSAD~UMSKZ ne 'F'
     and   BSAD~BELNR ne  BSAD~AUGBL.
    "
    " Estorno cliente
    select  BSAD~BUKRS BSAD~AUGDT BSAD~HKONT BSAD~AUGBL BSAD~AUGGJ
            BSAD~BELNR BSAD~GJAHR BSAD~BUZEI BSAD~BUDAT BSAD~WAERS BSAD~GSBER BSAD~WRBTR
            BSAD~DMBTR BSAD~DMBE2 BSAD~SGTXT BSAD~SHKZG
      from BSAD
      inner join BKPF
      on  BKPF~BUKRS = BSAD~BUKRS
      and BKPF~BELNR = BSAD~BELNR
      and BKPF~GJAHR = BSAD~GJAHR
      and BKPF~STBLG = BSAD~AUGBL
      into table IT_BSAD_EST
      for all entries in IT_SKB1
      where BSAD~BUKRS eq  P_BUKRS
      and   BSAD~BUDAT in P_AUGDT
      and   ( BSAD~AUGBL in P_BELNR or BSAD~BELNR in P_BELNR )
      and   BSAD~HKONT eq IT_SKB1-SAKNR
      and   BSAD~WAERS ne 'BRL'
      and   BSAD~BLART ne 'VC'
      and   BSAD~UMSKZ ne 'F'
      and   BSAD~BELNR ne  BSAD~AUGBL.

    " fornecedor
    select  BSAK~BUKRS BSAK~AUGDT BSAK~HKONT BSAK~AUGBL BSAK~AUGGJ
          BSAK~BELNR BSAK~GJAHR BSAK~BUZEI BSAK~BUDAT BSAK~WAERS BSAK~GSBER BSAK~WRBTR
          BSAK~DMBTR BSAK~DMBE2 BSAK~SGTXT BSAK~SHKZG BKPF~STBLG BKPF~STJAH
    from BSAK
    inner join BKPF
    on  BKPF~BUKRS = BSAK~BUKRS
    and BKPF~BELNR = BSAK~BELNR
    and BKPF~GJAHR = BSAK~GJAHR
    appending table IT_BSAD
    for all entries in IT_SKB1
    where BSAK~BUKRS eq  P_BUKRS
    and   BSAK~AUGDT in P_AUGDT
    and   ( BSAK~AUGBL in P_BELNR or  BSAK~BELNR in P_BELNR )
    and   BSAK~HKONT eq IT_SKB1-SAKNR
    and   BSAK~WAERS ne 'BRL'
    and   BSAK~BLART ne 'VC'
    and   BSAK~UMSKZ ne 'F'
    and   BSAK~BELNR ne  BSAK~AUGBL.

    " Estorno fornecedor
    select  BSAK~BUKRS BSAK~AUGDT BSAK~HKONT BSAK~AUGBL BSAK~AUGGJ
            BSAK~BELNR BSAK~GJAHR BSAK~BUZEI BSAK~BUDAT BSAK~WAERS BSAK~GSBER BSAK~WRBTR
            BSAK~DMBTR BSAK~DMBE2 BSAK~SGTXT BSAK~SHKZG
      from BSAK
      inner join BKPF
      on  BKPF~BUKRS = BSAK~BUKRS
      and BKPF~BELNR = BSAK~BELNR
      and BKPF~GJAHR = BSAK~GJAHR
      and BKPF~STBLG = BSAK~AUGBL
      appending table IT_BSAD_EST
      for all entries in IT_SKB1
      where BSAK~BUKRS eq  P_BUKRS
      and   BSAK~BUDAT in P_AUGDT
      and   ( BSAK~AUGBL in P_BELNR or  BSAK~BELNR in P_BELNR )
      and   BSAK~HKONT eq IT_SKB1-SAKNR
      and   BSAK~WAERS ne 'BRL'
      and   BSAK~BLART ne 'VC'
      and   BSAK~UMSKZ ne 'F'
      and   BSAK~BELNR ne  BSAK~AUGBL.

    "selecionar apenas documentos de estorno
    IT_BSAD_ESTD[] = IT_BSAD[].
    delete IT_BSAD_ESTD  where STJAH is initial.
    delete IT_BSAD       where STJAH is not initial.
    "
    "estorno
    if IT_BSAD_ESTD[] is not initial.
      select BUKRS_CLR BELNR_CLR GJAHR_CLR BELNR GJAHR BUZEI RDIFF SHKZG
      from BSE_CLR
      into table IT_BSE_ESTD
      for all entries in IT_BSAD_ESTD
      where BUKRS_CLR eq IT_BSAD_ESTD-BUKRS
      and   BELNR_CLR eq IT_BSAD_ESTD-BELNR
      and   GJAHR_CLR eq IT_BSAD_ESTD-GJAHR.

      if IT_BSE_ESTD[] is not initial.
        " Cliente
        select  BUKRS AUGDT HKONT AUGBL AUGGJ BELNR GJAHR BUZEI BUDAT WAERS GSBER WRBTR DMBTR DMBE2 SGTXT SHKZG AUGBL
         from BSAD
         appending table IT_BSAD
         for all entries in IT_BSE_ESTD
         where BUKRS eq  P_BUKRS
         and   BELNR eq IT_BSE_ESTD-BELNR
         and   GJAHR eq IT_BSE_ESTD-GJAHR
         and   BUZEI eq IT_BSE_ESTD-BUZEI
         and   WAERS ne 'BRL'
         and   BLART ne 'VC'
         and   UMSKZ ne 'F'
         and   BSAD~BELNR ne  BSAD~AUGBL.

        " fornecedor
        select  BUKRS AUGDT HKONT AUGBL AUGGJ BELNR GJAHR BUZEI BUDAT WAERS GSBER WRBTR DMBTR DMBE2 SGTXT SHKZG AUGBL
         from BSAK
         appending table IT_BSAD
         for all entries in IT_BSE_ESTD
         where BUKRS eq  P_BUKRS
         and   BELNR eq IT_BSE_ESTD-BELNR
         and   GJAHR eq IT_BSE_ESTD-GJAHR
         and   BUZEI eq IT_BSE_ESTD-BUZEI
         and   WAERS ne 'BRL'
         and   BLART ne 'VC'
         and   UMSKZ ne 'F'
         and   BSAK~BELNR ne  BSAK~AUGBL.
      endif.
    endif.

    if IT_BSAD_EST[] is not initial.
      select BUKRS_CLR BELNR_CLR GJAHR_CLR BELNR GJAHR BUZEI RDIFF SHKZG
      from BSE_CLR
      into table IT_BSE_EST
      for all entries in IT_BSAD_EST
      where BUKRS_CLR eq IT_BSAD_EST-BUKRS
      and   BELNR_CLR eq IT_BSAD_EST-BELNR
      and   GJAHR_CLR eq IT_BSAD_EST-GJAHR.

      if IT_BSE_EST[] is not initial.
        " Cliente
        select  BUKRS AUGDT HKONT AUGBL AUGGJ BELNR GJAHR BUZEI BUDAT WAERS GSBER WRBTR DMBTR DMBE2 SGTXT SHKZG
         from BSAD
         appending table IT_BSAD
         for all entries in IT_BSE_EST
         where BUKRS eq  P_BUKRS
         and   BELNR eq IT_BSE_EST-BELNR
         and   GJAHR eq IT_BSE_EST-GJAHR
         and   BUZEI eq IT_BSE_EST-BUZEI
         and   WAERS ne 'BRL'
         and   BLART ne 'VC'
         and   UMSKZ ne 'F'
         and   BSAD~BELNR ne  BSAD~AUGBL.

        " fornecedor
        select  BUKRS AUGDT HKONT AUGBL AUGGJ BELNR GJAHR BUZEI BUDAT WAERS GSBER WRBTR DMBTR DMBE2 SGTXT SHKZG
         from BSAK
         appending table IT_BSAD
         for all entries in IT_BSE_EST
         where BUKRS eq  P_BUKRS
         and   BELNR eq IT_BSE_EST-BELNR
         and   GJAHR eq IT_BSE_EST-GJAHR
         and   BUZEI eq IT_BSE_EST-BUZEI
         and   WAERS ne 'BRL'
         and   BLART ne 'VC'
         and   UMSKZ ne 'F'
         and   BSAK~BELNR ne  BSAK~AUGBL.
      endif.
    endif.

  endif.

  if IT_BSAD[] is not initial.
    "
    select BUKRS_CLR BELNR_CLR GJAHR_CLR BELNR GJAHR BUZEI RDIFF SHKZG
      from BSE_CLR
      into table IT_BSE
      for all entries in IT_BSAD
      where BUKRS_CLR eq IT_BSAD-BUKRS
      and   BELNR_CLR eq IT_BSAD-AUGBL
      and   GJAHR_CLR eq IT_BSAD-AUGGJ
      and   BELNR     eq IT_BSAD-BELNR
      and   GJAHR     eq IT_BSAD-GJAHR
      and   BUZEI     eq IT_BSAD-BUZEI.
    "docs de compe
    select BUKRS BELNR GJAHR KURSF KURS2
      from BKPF
      into table IT_BKPF
      for all entries in IT_BSAD
      where BUKRS eq IT_BSAD-BUKRS
      and   BELNR eq IT_BSAD-AUGBL
      and   GJAHR eq IT_BSAD-AUGGJ.
    "
    select BUKRS BELNR GJAHR KURSF KURS2 STBLG STJAH BUDAT
       from BKPF
       appending table IT_BKPF
       for all entries in IT_BSAD
       where BUKRS eq IT_BSAD-BUKRS
       and   BELNR eq IT_BSAD-BELNR
       and   GJAHR eq IT_BSAD-GJAHR.

    select  BUKRS BELNR BUZEI DT_AVAL VLR_ACUM_MES_ATU
      from ZGL012_AVM
      into table IT_ZGL012
      for all entries in IT_BSAD
       where BUKRS eq IT_BSAD-BUKRS
       and   BELNR eq IT_BSAD-BELNR
       and   BUZEI eq IT_BSAD-BUZEI.
  endif.

  " Sigam_financiamento_produtor
  select *
    from ZFIT0132
    into table IT_ZFIT0132
    where ID_EMPRESA_SAP eq P_BUKRS
    and   DATA_BAIXA in P_AUGDT
    and   DOC_SAP_COMPENSACAO in P_BELNR.

  if IT_ZFIT0132[] is not initial.
    select   BUKRS BELNR AUGBL SHKZG HKONT SGTXT
      from BSAK
      into table IT_BSAK
      for all entries in IT_ZFIT0132
      where BUKRS    eq IT_ZFIT0132-ID_EMPRESA_SAP
      and   BELNR    eq IT_ZFIT0132-DOC_SAP_ORIGEM
      and   AUGBL    eq IT_ZFIT0132-DOC_SAP_COMPENSACAO
      and   AUGBL    in P_BELNR.

    if IT_BSAK[] is not initial.
      select SAKNR TXT50
         from SKAT
         appending table IT_SKAT
         for all entries in IT_BSAK
         where SAKNR eq IT_BSAK-HKONT
         and   SPRAS eq SY-LANGU
         and   KTOPL eq '0050'.

      select SAKNR MITKZ "#EC CI_DB_OPERATION_OK[2431747]
        from SKB1
        appending table IT_SKB1
        for all entries in IT_BSAK
        where BUKRS eq P_BUKRS
        and   SAKNR eq IT_BSAK-HKONT
        and   SAKNR in P_HKONT.
    endif.
  endif.

  " XRT_financiamento
  select *
    from ZFIT0133
    into table IT_ZFIT0133_LIQ
    where ID_EMPRESA_SAP eq P_BUKRS
    and   DATA_BAIXA     in P_AUGDT
    and   CREDITO_DEBITO eq 'D'
    and   APROPRIACAO_CAIXA eq 'C'.

  if IT_ZFIT0133_LIQ[] is not initial.
    select *
      from ZFIT0133
      into table IT_ZFIT0133_CAP
      for all entries in IT_ZFIT0133_LIQ
      where CONTRATO_NUMERO   eq IT_ZFIT0133_LIQ-CONTRATO_NUMERO
      and   CREDITO_DEBITO    eq 'C'
      and   APROPRIACAO_CAIXA eq 'A'
      and   TIPO              eq 'JUR'.
    "
    select *
      from ZFIT0133
      appending table IT_ZFIT0133_CAP
      for all entries in IT_ZFIT0133_LIQ
      where CONTRATO_NUMERO   eq IT_ZFIT0133_LIQ-CONTRATO_NUMERO
      and   CREDITO_DEBITO    eq 'C'
      and   APROPRIACAO_CAIXA eq 'C'
      and   TIPO              eq 'PRI'.

    "CHAVE CAPTAÇÃO
    loop at IT_ZFIT0133_CAP into WA_ZFIT0133_CAP.
      TABIX = SY-TABIX.
      concatenate WA_ZFIT0133_CAP-CHAVE_REFERENCIA '%' into WA_ZFIT0133_CAP-CHAVE_REFERENCIA.
      "
      select  OBJ_KEY BUKRS BELNR GJAHR
      from ZIB_CONTABIL_CHV
      into   WA_ZIB_CONTABIL_CHV
      where OBJ_KEY like WA_ZFIT0133_CAP-CHAVE_REFERENCIA.
        WA_ZIB_CONTABIL_CHV-XREF1 = WA_ZFIT0133_CAP-PAR_CONTABIL.
        select single   *
          from BSAK
          into @data(WBSAK)
           where BUKRS    eq @WA_ZIB_CONTABIL_CHV-BUKRS
           and   BELNR    eq @WA_ZIB_CONTABIL_CHV-BELNR
           and   GJAHR    eq @WA_ZIB_CONTABIL_CHV-GJAHR
           and   XREF1    eq @WA_ZIB_CONTABIL_CHV-XREF1.
        if SY-SUBRC eq 0.
          WA_ZFIT0133_CAP-CHAVE_REFERENCIA = WA_ZIB_CONTABIL_CHV-OBJ_KEY.
          modify IT_ZFIT0133_CAP from WA_ZFIT0133_CAP index TABIX transporting CHAVE_REFERENCIA.
          append WA_ZIB_CONTABIL_CHV to IT_ZIB_CONTABIL_CHV.
          exit.
        endif.
      endselect.


    endloop.

    "CHAVE LIQUIDAÇÃO
    loop at IT_ZFIT0133_LIQ into WA_ZFIT0133_LIQ.
      TABIX = SY-TABIX.
      concatenate WA_ZFIT0133_LIQ-CHAVE_REFERENCIA '%' into WA_ZFIT0133_LIQ-CHAVE_REFERENCIA.
      select  OBJ_KEY BUKRS BELNR GJAHR
          from ZIB_CONTABIL_CHV
          into   WA_ZIB_CONTABIL_CHV
          where OBJ_KEY like WA_ZFIT0133_LIQ-CHAVE_REFERENCIA.
        WA_ZIB_CONTABIL_CHV-XREF1 = WA_ZFIT0133_LIQ-PAR_CONTABIL.
        select single   *
          from BSAK
          into @data(WBSAK2)
           where BUKRS    eq @WA_ZIB_CONTABIL_CHV-BUKRS
           and   BELNR    eq @WA_ZIB_CONTABIL_CHV-BELNR
           and   GJAHR    eq @WA_ZIB_CONTABIL_CHV-GJAHR
           and   XREF1    eq @WA_ZIB_CONTABIL_CHV-XREF1.
        if SY-SUBRC eq 0.
          WA_ZFIT0133_LIQ-CHAVE_REFERENCIA = WA_ZIB_CONTABIL_CHV-OBJ_KEY.
          modify IT_ZFIT0133_LIQ from WA_ZFIT0133_LIQ index TABIX transporting CHAVE_REFERENCIA.
          append WA_ZIB_CONTABIL_CHV to IT_ZIB_CONTABIL_CHV.
          exit.
        endif.
      endselect.

    endloop.

    if IT_ZFIT0133_CAP[] is not initial.
      select *
        from ZFIT0134
        into table IT_ZFIT0134
        for all entries in IT_ZFIT0133_CAP
        where CONTRATO_NUMERO eq IT_ZFIT0133_CAP-CONTRATO_NUMERO.

      "Captações documentos SAP ENTRADA PRI
      select *
        from ZFIT0135
        into table IT_ZFIT0135
        for all entries in IT_ZFIT0133_CAP
        where CONTRATO_NUMERO = IT_ZFIT0133_CAP-CONTRATO_NUMERO
        and   ENT_SAI         eq 'E'
        and   BELNR           ne ''.

      "Liquidações documentos SAP SAIDA PRI
      select *
        from ZFIT0135
        appending table IT_ZFIT0135
        for all entries in IT_ZFIT0133_LIQ
        where CONTRATO_NUMERO = IT_ZFIT0133_LIQ-CONTRATO_NUMERO
        and   ENT_SAI         eq 'S'
        and   BELNR           ne ''.

      if IT_ZFIT0135[] is not initial.
        select   BUKRS BELNR AUGBL SHKZG HKONT SGTXT GJAHR XREF1
            from BSAK
            appending table IT_BSAK
            for all entries in IT_ZFIT0135
            where BUKRS    eq IT_ZFIT0135-BUKRS
            and   BELNR    eq IT_ZFIT0135-BELNR
            and   GJAHR    eq IT_ZFIT0135-GJAHR.
      endif.

    endif.

    if IT_ZIB_CONTABIL_CHV[] is not initial.
      select   BUKRS BELNR AUGBL SHKZG HKONT SGTXT GJAHR XREF1
        from BSAK
        appending table IT_BSAK
        for all entries in IT_ZIB_CONTABIL_CHV
        where BUKRS    eq IT_ZIB_CONTABIL_CHV-BUKRS
        and   BELNR    eq IT_ZIB_CONTABIL_CHV-BELNR
        and   GJAHR    eq IT_ZIB_CONTABIL_CHV-GJAHR.
    endif.

    if IT_BSAK[] is not initial.
* ---> S4 Migration - 15/06/2023 - MA
*      select  BUKRS BELNR GJAHR AUGBL BEWAR
*        from BSEG
*        into table IT_BSEG
*        for all entries in IT_BSAK
*        where BUKRS eq IT_BSAK-BUKRS
*        and   BELNR eq IT_BSAK-BELNR
*        and   GJAHR eq IT_BSAK-GJAHR
*        and   AUGBL eq IT_BSAK-AUGBL
*        and   BEWAR in ( 'Z02', 'Z06' ).

      data LT_FIELDS type FAGL_T_FIELD.
      data: LT_BSEG type table of BSEG,
            TBSEG   type table of BSEG.
      types LR_BEWAR_TYPE type range of BEWAR.
      data : LR_BEWAR type LR_BEWAR_TYPE.

      LT_FIELDS = value #( ( LINE = 'BUKRS' )
                           ( LINE = 'BELNR' )
                           ( LINE = 'GJAHR' )
                           ( LINE = 'AUGBL' )
                           ( LINE = 'BEWAR' )
                           ).

      LR_BEWAR = value LR_BEWAR_TYPE( let S = 'I'
                                          O = 'BT'
                                      in SIGN   = S
                                         OPTION = O
                                         ( LOW = 'Z02' )
                                         ( LOW = 'Z06' ) ).

      call function 'FAGL_GET_BSEG_FOR_ALL_ENTRIES'
        exporting
          IT_FOR_ALL_ENTRIES = IT_BSAK
          I_WHERE_CLAUSE     = |BUKRS = IT_FOR_ALL_ENTRIES-BUKRS AND BELNR = IT_FOR_ALL_ENTRIES-BELNR AND GJAHR = IT_FOR_ALL_ENTRIES-GJAHR AND AUGBL = IT_FOR_ALL_ENTRIES- AUGBL|
          IT_FIELDLIST       = LT_FIELDS
        importing
          ET_BSEG            = LT_BSEG
        exceptions
          NOT_FOUND          = 1.

      delete LT_BSEG where BEWAR not in LR_BEWAR.

      if SY-SUBRC = 0 and LINES( LT_BSEG ) > 0.
        move-corresponding LT_BSEG to IT_BSEG.
        SY-DBCNT = LINES( LT_BSEG ).
      else.
        SY-SUBRC = 4.
        SY-DBCNT = 0.
      endif.
* <--- S4 Migration - 15/06/2023 - MA



      select SAKNR TXT50
         from SKAT
         appending table IT_SKAT
         for all entries in IT_BSAK
         where SAKNR eq IT_BSAK-HKONT
         and   SPRAS eq SY-LANGU
         and   KTOPL eq '0050'.

      select SAKNR MITKZ "#EC CI_DB_OPERATION_OK[2431747]
        from SKB1
        appending table IT_SKB1
        for all entries in IT_BSAK
        where BUKRS eq P_BUKRS
        and   SAKNR eq IT_BSAK-HKONT
        and   SAKNR in P_HKONT.
    endif.
  endif.
endform.


form F_PROCESSAR_DADOS.

  sort: IT_BKPF             by BUKRS BELNR GJAHR,
        IT_BSAK             by BUKRS BELNR GJAHR,
        IT_BSAD_EST         by BUKRS BELNR GJAHR,
        IT_BSAD_ESTD        by BUKRS BELNR GJAHR,
        IT_BSEG             by BUKRS BELNR GJAHR AUGBL,
        IT_BSE_EST          by BUKRS_CLR BELNR GJAHR BUZEI,
        IT_BSE_ESTD         by BUKRS_CLR BELNR GJAHR BUZEI,
        IT_ZGL012           by BUKRS BELNR BUZEI ascending DT_AVAL descending,
        IT_ZIB_CONTABIL_CHV by OBJ_KEY,
        IT_SKAT             by SAKNR,
        IT_SKB1             by SAKNR,
        IT_ZFIT0133_CAP     by CONTRATO_NUMERO TIPO ascending DATA_BAIXA descending ,
        IT_ZFIT0133_LIQ     by CONTRATO_NUMERO TIPO ascending DATA_BAIXA descending ,
        IT_ZFIT0134         by CONTRATO_NUMERO ENT_SAI DATA_EFETIVA,
        IT_ZFIT0135         by CONTRATO_NUMERO DATA_EFETIVA ENT_SAI,
        IT_BSE              by BUKRS_CLR BELNR_CLR GJAHR_CLR BELNR GJAHR BUZEI.

  data: VVALOR_ORIGINAL_D type ZFIT0133-VALOR_ORIGINAL_D,
        IT_CONTAS         type ZCT_EMP_CONTAS,
        WA_CONTAS         type ZLC_EMP_CONTAS,
        W_LINES           type SY-TABIX,
        WA_T030H          type T030H,
        IT_BKPF_          type table of BKPF,
        WA_BKPF_          type BKPF,

        IT_PARTIDAS       type table of ZDE_FI_GL_PARTIDAS_CLI_FOR with header line.

  WA_CONTAS-BUKRS   = P_BUKRS.
  WA_CONTAS-SAKNR = '00006*'.
  append WA_CONTAS to IT_CONTAS.

  " XRT
  loop at IT_ZFIT0133_LIQ into WA_ZFIT0133_LIQ.
    read table IT_ZIB_CONTABIL_CHV into WA_ZIB_CONTABIL_CHV with key OBJ_KEY = WA_ZFIT0133_LIQ-CHAVE_REFERENCIA binary search.
    if SY-SUBRC ne 0.
      continue.
    endif.

    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        INPUT  = WA_ZFIT0133_LIQ-CONTA_CONTABIL
      importing
        OUTPUT = WA_ZFIT0133_LIQ-CONTA_CONTABIL.

    select single *
    from ZFIT0131
    into WA_ZFIT0131
    where ID_SISTEMA = '3' " XRT
    and   HKONT      = WA_ZFIT0133_LIQ-CONTA_CONTABIL.

    if SY-SUBRC ne 0.
      continue.
    endif.
    "juros
    VVALOR_ORIGINAL_D = WA_ZFIT0133_LIQ-VALOR_ORIGINAL_D.
    loop at IT_ZFIT0133_CAP into WA_ZFIT0133_CAP where CONTRATO_NUMERO = WA_ZFIT0133_LIQ-CONTRATO_NUMERO.
      "
*      MULTIPLY WA_ZFIT0133_CAP-VALOR_ORIGINAL_R BY -1.
*      MULTIPLY WA_ZFIT0133_CAP-VALOR_ORIGINAL_D BY -1.
      "
      clear WA_SAIDA_0100.
      if WA_ZFIT0133_LIQ-TIPO = 'JUR' and WA_ZFIT0133_CAP-TIPO ne 'JUR'.
        continue.
      endif.

      if ( WA_ZFIT0133_LIQ-TIPO = 'PRI' and WA_ZFIT0133_CAP-TIPO ne 'PRI') or
         ( WA_ZFIT0133_LIQ-DATA_BAIXA not in P_AUGDT and WA_ZFIT0133_LIQ-TIPO = 'PRI' ).
        continue.
      endif.

      clear: WA_ZIB_CONTABIL_CHV, WA_ZFIT0135.
      "liquidação
      if WA_ZFIT0133_LIQ-TIPO = 'PRI'.
        clear WA_ZFIT0135.
        read table IT_ZFIT0135 into WA_ZFIT0135 with key CONTRATO_NUMERO = WA_ZFIT0133_CAP-CONTRATO_NUMERO
                                                         ENT_SAI         = 'E' binary search.
        WA_ZIB_CONTABIL_CHV-BUKRS = WA_ZFIT0135-BUKRS.
        WA_ZIB_CONTABIL_CHV-BELNR = WA_ZFIT0135-BELNR.
        WA_ZIB_CONTABIL_CHV-GJAHR = WA_ZFIT0135-GJAHR.
        WA_SAIDA_0100-BELNR       = WA_ZIB_CONTABIL_CHV-BELNR.
      else.
        read table IT_ZIB_CONTABIL_CHV into WA_ZIB_CONTABIL_CHV with key OBJ_KEY = WA_ZFIT0133_CAP-CHAVE_REFERENCIA binary search.
        WA_SAIDA_0100-BELNR       = WA_ZIB_CONTABIL_CHV-BELNR.
*        READ TABLE IT_BSEG INTO WA_BSEG WITH KEY BUKRS = WA_ZIB_CONTABIL_CHV-BUKRS
*                                                 BELNR = WA_ZIB_CONTABIL_CHV-BELNR
*                                                 GJAHR = WA_ZIB_CONTABIL_CHV-GJAHR  BINARY SEARCH.
*        IF SY-SUBRC NE 0.
*          CONTINUE.
*        ENDIF.
      endif.


      clear WA_BSAK.
      if WA_ZIB_CONTABIL_CHV-BUKRS is not initial.
        read table IT_BSAK into WA_BSAK with key BUKRS = WA_ZIB_CONTABIL_CHV-BUKRS
                                                 BELNR = WA_ZIB_CONTABIL_CHV-BELNR
                                                 GJAHR = WA_ZIB_CONTABIL_CHV-GJAHR binary search.

        if P_HKONT is not initial.
          if WA_BSAK-HKONT not in P_HKONT.
            continue.
          endif.
        endif.

        read table IT_ZIB_CONTABIL_CHV into WA_ZIB_CONTABIL_CHV with key OBJ_KEY = WA_ZFIT0133_LIQ-CHAVE_REFERENCIA binary search.
        read table IT_BSEG into WA_BSEG with key BUKRS = WA_ZIB_CONTABIL_CHV-BUKRS
                                                 BELNR = WA_ZIB_CONTABIL_CHV-BELNR
                                                 GJAHR = WA_ZIB_CONTABIL_CHV-GJAHR  binary search.
        if SY-SUBRC ne 0.
          continue.
        endif.

        read table IT_SKB1 into WA_SKB1 with key SAKNR = WA_BSAK-HKONT binary search.

        select single *
            from ZFIT0131
            into WA_ZFIT0131
            where ID_SISTEMA = '3' " XRT
            and   HKONT      = WA_BSAK-HKONT.

        if SY-SUBRC ne 0.
          continue.
        endif.

        clear WA_SKAT.
        read table IT_SKAT into WA_SKAT with key SAKNR = WA_BSAK-HKONT binary search.
        WA_SAIDA_0100-TXT50       = WA_SKAT-TXT50.
      endif.

      WA_SAIDA_0100-HKONT       = WA_BSAK-HKONT.

      if WA_ZFIT0133_LIQ-TIPO = 'JUR'.
        WA_SAIDA_0100-GSBER       = WA_ZFIT0133_CAP-ID_FILIAL_SAP.
        WA_SAIDA_0100-BUDAT       = WA_ZFIT0133_CAP-DATA_BAIXA.

        WA_SAIDA_0100-ANOMEL      = WA_ZFIT0133_CAP-DATA_BAIXA+0(6).
        concatenate WA_SAIDA_0100-ANOMEL+0(4) '.' WA_SAIDA_0100-ANOMEL+4(2) into WA_SAIDA_0100-ANOMEL.

        WA_SAIDA_0100-DMBTR       = WA_ZFIT0133_CAP-VALOR_ORIGINAL_R.
        WA_SAIDA_0100-DMBE2       = WA_ZFIT0133_CAP-VALOR_ORIGINAL_D.
        WA_SAIDA_0100-WRBTR       = 0.
        WA_SAIDA_0100-WAERS       = ''.
      else.
        WA_SAIDA_0100-GSBER       = WA_ZFIT0133_LIQ-ID_FILIAL_SAP.
        WA_SAIDA_0100-BUDAT       = WA_ZFIT0133_LIQ-DATA_BAIXA.

        WA_SAIDA_0100-ANOMEL      = WA_ZFIT0133_LIQ-DATA_BAIXA+0(6).
        concatenate WA_SAIDA_0100-ANOMEL+0(4) '.' WA_SAIDA_0100-ANOMEL+4(2) into WA_SAIDA_0100-ANOMEL.

        WA_SAIDA_0100-DMBTR       = WA_ZFIT0133_LIQ-VALOR_ORIGINAL_R.
        WA_SAIDA_0100-DMBE2       = WA_ZFIT0133_LIQ-VALOR_ORIGINAL_D.
        WA_SAIDA_0100-WRBTR       = 0.
        WA_SAIDA_0100-WAERS       = ''.
      endif.

      clear WA_ZIB_CONTABIL_CHV.
      read table IT_ZIB_CONTABIL_CHV into WA_ZIB_CONTABIL_CHV with key OBJ_KEY = WA_ZFIT0133_LIQ-CHAVE_REFERENCIA binary search.
      if SY-SUBRC ne 0.
        clear WA_ZFIT0135.
        read table IT_ZFIT0135 into WA_ZFIT0135 with key CONTRATO_NUMERO = WA_ZFIT0133_LIQ-CONTRATO_NUMERO
                                                         DATA_EFETIVA    = WA_ZFIT0133_LIQ-DATA_BAIXA
                                                         ENT_SAI         = 'S' binary search.
        WA_ZIB_CONTABIL_CHV-BUKRS = WA_ZFIT0135-BUKRS.
        WA_ZIB_CONTABIL_CHV-BELNR = WA_ZFIT0135-BELNR.
        WA_ZIB_CONTABIL_CHV-GJAHR = WA_ZFIT0135-GJAHR.
      endif.
      if WA_ZIB_CONTABIL_CHV-BELNR is not initial.
        refresh: IT_BKPF_, IT_PARTIDAS.
        WA_BKPF_-BUKRS = P_BUKRS.
        WA_BKPF_-BELNR = WA_ZIB_CONTABIL_CHV-BELNR.
        WA_BKPF_-GJAHR = WA_ZIB_CONTABIL_CHV-GJAHR.
        append WA_BKPF_ to IT_BKPF_.

        call function 'Z_FI_GL_PARTIDAS'
          exporting
            I_CONTAS    = IT_CONTAS
          tables
            IT_BKPF     = IT_BKPF_
            IT_PARTIDAS = IT_PARTIDAS.
        if IT_PARTIDAS[] is not initial.
          W_LINES =  LINES( IT_PARTIDAS ).
          if  W_LINES = 1.
            read table IT_PARTIDAS index 1.
            WA_SAIDA_0100-CONTA_VC    = IT_PARTIDAS-HKONT.
          else.
            select single *
              from T030H
              into WA_T030H
              where KTOPL = '0050'
              and   HKONT = WA_SAIDA_0100-HKONT.
            loop at IT_PARTIDAS.
              if IT_PARTIDAS-HKONT = WA_T030H-LSREA.
                WA_SAIDA_0100-CONTA_VC    = IT_PARTIDAS-HKONT.
              endif.
            endloop.
          endif.
        endif.
      endif.

      "liquidação
      if WA_ZFIT0133_LIQ-TIPO = 'PRI'.
        read table IT_ZFIT0134 into WA_ZFIT0134 with key CONTRATO_NUMERO = WA_ZFIT0133_CAP-CONTRATO_NUMERO
                                                         ENT_SAI         = 'E' binary search.
        if SY-SUBRC = 0.
          WA_SAIDA_0100-BUDAT       = WA_ZFIT0134-DATA_EFETIVA.
          WA_SAIDA_0100-AUGBL       = WA_ZIB_CONTABIL_CHV-BELNR.

          WA_SAIDA_0100-AUGDT       = WA_ZFIT0133_LIQ-DATA_BAIXA.
          WA_SAIDA_0100-ANOMEC      = WA_ZFIT0133_LIQ-DATA_BAIXA+0(6).

          concatenate WA_SAIDA_0100-ANOMEC+0(4) '.' WA_SAIDA_0100-ANOMEC+4(2) into WA_SAIDA_0100-ANOMEC.

          WA_SAIDA_0100-PTAXCA      = WA_ZFIT0134-TAXA. "Taxa de captação
          WA_SAIDA_0100-PTAXCO      = WA_ZFIT0133_LIQ-VR_COTACAO.
        endif.
      else.
        if WA_ZFIT0133_CAP-DATA_BAIXA le WA_ZFIT0133_LIQ-DATA_BAIXA.
          VVALOR_ORIGINAL_D = VVALOR_ORIGINAL_D - WA_ZFIT0133_CAP-VALOR_ORIGINAL_D.
          if VVALOR_ORIGINAL_D ge 0.
            WA_SAIDA_0100-AUGBL       = WA_ZIB_CONTABIL_CHV-BELNR.
            WA_SAIDA_0100-AUGDT       = WA_ZFIT0133_LIQ-DATA_BAIXA.
            WA_SAIDA_0100-ANOMEC      = WA_ZFIT0133_LIQ-DATA_BAIXA+0(6).
            concatenate WA_SAIDA_0100-ANOMEC+0(4) '.' WA_SAIDA_0100-ANOMEC+4(2) into WA_SAIDA_0100-ANOMEC.

            WA_SAIDA_0100-PTAXCA      = WA_ZFIT0133_CAP-VR_COTACAO.
            WA_SAIDA_0100-PTAXCO      = WA_ZFIT0133_LIQ-VR_COTACAO.
          else.
            continue.
          endif.
        else.
          continue.
        endif.
      endif.

      if P_BELNR is not initial.
        if WA_SAIDA_0100-AUGBL not in P_BELNR.
          continue.
        endif.
      endif.

      "Cálculo  VC Contábil
      WA_SAIDA_0100-VL_CTB      = ( WA_SAIDA_0100-PTAXCO - WA_SAIDA_0100-PTAXCA ) * WA_SAIDA_0100-DMBE2.

      "Cálculo VC Fiscal
      if WA_SAIDA_0100-BUDAT le WA_ZFIT0130-DT_TX_FIXADA.
        WA_SAIDA_0100-VL_FIS      = ( WA_SAIDA_0100-PTAXCO - WA_ZFIT0130-TX_FIXADA ) *  WA_SAIDA_0100-DMBE2.
      else.
        WA_SAIDA_0100-VL_FIS      = ( WA_SAIDA_0100-PTAXCO - WA_SAIDA_0100-PTAXCA ) *  WA_SAIDA_0100-DMBE2.
      endif.

      if WA_BSAK-SHKZG = 'H'.
        if WA_SAIDA_0100-VL_FIS gt 0.
          WA_SAIDA_0100-REC_DES     = 'Despesa'.
        else.
          WA_SAIDA_0100-REC_DES     = 'Receita'.
        endif.
      endif.

      if WA_BSAK-SHKZG = 'S'.
        if WA_SAIDA_0100-VL_FIS gt 0.
          WA_SAIDA_0100-REC_DES     = 'Despesa'.
        else.
          WA_SAIDA_0100-REC_DES     = 'Receita'.
        endif.
      endif.

      " Dif. VC Cont. x VC. Fiscal
      WA_SAIDA_0100-DIF_CTB_FIS = WA_SAIDA_0100-VL_CTB - WA_SAIDA_0100-VL_FIS.

      "VC Cx doc Contábil
      WA_SAIDA_0100-VL_CTB_CX   = 0.

      "VC CTB-VC DCTO Contábil
      WA_SAIDA_0100-VL_CTB_DOC  = 0.
      " WA_SAIDA_0100-VL_CTB_DOC  = WA_SAIDA_0100-VL_CTB - WA_SAIDA_0100-VL_CTB_CX.

      "VC competencia
      WA_SAIDA_0100-VL_CTB_CP   = 0.

      WA_SAIDA_0100-SGTXT       = WA_BSAK-SGTXT.
      WA_SAIDA_0100-ORIGEM      = 'XRT'.

      append WA_SAIDA_0100 to IT_SAIDA_0100.
      if ( WA_ZFIT0133_LIQ-TIPO = 'PRI' and WA_ZFIT0133_CAP-TIPO eq 'PRI').
        exit.
      endif.
    endloop.

  endloop.
  "

  sort IT_BSAK  by BUKRS BELNR AUGBL.
  " Produtor
  loop at IT_ZFIT0132 into WA_ZFIT0132.
    clear WA_SAIDA_0100.
    WA_SAIDA_0100-BELNR       = WA_ZFIT0132-DOC_SAP_ORIGEM.


    clear WA_BSAK.
    read table IT_BSAK into WA_BSAK with key BUKRS = WA_ZFIT0132-ID_EMPRESA_SAP
                                             BELNR = WA_ZFIT0132-DOC_SAP_ORIGEM
                                             AUGBL = WA_ZFIT0132-DOC_SAP_COMPENSACAO binary search.
    if SY-SUBRC ne 0.
      select single *
        from BSAK
        into corresponding fields of WA_BSAK
        where BUKRS = WA_ZFIT0132-ID_EMPRESA_SAP
        and   BELNR = WA_ZFIT0132-DOC_SAP_ORIGEM
        and   GJAHR = WA_ZFIT0132-DATA_ORIGEM+0(4)
        and   SHKZG = 'S'.
      WA_BSAK-SHKZG = 'S'.
    endif.


    if P_HKONT is not initial.
      if WA_BSAK-HKONT not in P_HKONT.
        continue.
      endif.
    endif.

    select single *
    from ZFIT0131
    into WA_ZFIT0131
    where ID_SISTEMA = '2' " Sigam
    and   HKONT      = WA_BSAK-HKONT.

    if SY-SUBRC ne 0.
      continue.
    endif.

    read table IT_SKB1 into WA_SKB1 with key SAKNR = WA_BSAK-HKONT binary search.

    clear WA_SKAT.
    read table IT_SKAT into WA_SKAT with key SAKNR = WA_BSAK-HKONT binary search.
    WA_SAIDA_0100-TXT50       = WA_SKAT-TXT50.

    multiply  WA_ZFIT0132-VALOR_BAIXA_REAL  by -1.
    multiply  WA_ZFIT0132-VALOR_BAIXA_DOLAR by -1.

    WA_SAIDA_0100-HKONT       = WA_BSAK-HKONT.
    WA_SAIDA_0100-GSBER       = WA_ZFIT0132-ID_FILIAL_SAP.
    WA_SAIDA_0100-BUDAT       = WA_ZFIT0132-DATA_ORIGEM.
    WA_SAIDA_0100-ANOMEL      = WA_ZFIT0132-DATA_ORIGEM+0(6).
    concatenate WA_SAIDA_0100-ANOMEL+0(4) '.' WA_SAIDA_0100-ANOMEL+4(2) into WA_SAIDA_0100-ANOMEL.

    WA_SAIDA_0100-DMBTR       = WA_ZFIT0132-VALOR_BAIXA_REAL.
    WA_SAIDA_0100-DMBE2       = WA_ZFIT0132-VALOR_BAIXA_DOLAR.
    WA_SAIDA_0100-WRBTR       = 0.
    WA_SAIDA_0100-WAERS       = ''.
    WA_SAIDA_0100-AUGBL       = WA_ZFIT0132-DOC_SAP_COMPENSACAO.
    WA_SAIDA_0100-AUGDT       = WA_ZFIT0132-DATA_BAIXA.
    WA_SAIDA_0100-ANOMEC      = WA_ZFIT0132-DATA_BAIXA+0(6).
    concatenate WA_SAIDA_0100-ANOMEC+0(4) '.' WA_SAIDA_0100-ANOMEC+4(2) into WA_SAIDA_0100-ANOMEC.

    WA_SAIDA_0100-PTAXCA      = WA_ZFIT0132-VR_COTACAO_ORIGEM.
    WA_SAIDA_0100-PTAXCO      = WA_ZFIT0132-VR_COTACAO_BAIXA.

    if WA_SAIDA_0100-AUGBL is not initial.
      refresh: IT_BKPF_, IT_PARTIDAS.
      WA_BKPF_-BUKRS = P_BUKRS.
      WA_BKPF_-BELNR = WA_SAIDA_0100-AUGBL.
      WA_BKPF_-GJAHR = WA_SAIDA_0100-AUGDT+0(4).
      append WA_BKPF_ to IT_BKPF_.

      call function 'Z_FI_GL_PARTIDAS'
        exporting
          I_CONTAS    = IT_CONTAS
        tables
          IT_BKPF     = IT_BKPF_
          IT_PARTIDAS = IT_PARTIDAS.

      if IT_PARTIDAS[] is not initial.
        W_LINES =  LINES( IT_PARTIDAS ).
        if  W_LINES = 1.
          read table IT_PARTIDAS index 1.
          WA_SAIDA_0100-CONTA_VC    = IT_PARTIDAS-HKONT.
        else.
          select single *
            from T030H
            into WA_T030H
            where KTOPL = '0050'
            and   HKONT = WA_SAIDA_0100-HKONT.
          loop at IT_PARTIDAS.
            if IT_PARTIDAS-HKONT = WA_T030H-LSREA.
              WA_SAIDA_0100-CONTA_VC    = IT_PARTIDAS-HKONT.
            endif.
          endloop.
        endif.
      endif.
    endif.

    if P_BELNR is not initial.
      if WA_SAIDA_0100-AUGBL not in P_BELNR.
        continue.
      endif.
    endif.

    "Cálculo  VC Contábil
    WA_SAIDA_0100-VL_CTB      = ( WA_SAIDA_0100-PTAXCO - WA_SAIDA_0100-PTAXCA ) * WA_SAIDA_0100-DMBE2.

    "Cálculo VC Fiscal
    if WA_SAIDA_0100-BUDAT le WA_ZFIT0130-DT_TX_FIXADA.
      WA_SAIDA_0100-VL_FIS      = ( WA_SAIDA_0100-PTAXCO - WA_ZFIT0130-TX_FIXADA ) *  WA_SAIDA_0100-DMBE2.
    else.
      WA_SAIDA_0100-VL_FIS      = ( WA_SAIDA_0100-PTAXCO - WA_SAIDA_0100-PTAXCA ) *  WA_SAIDA_0100-DMBE2.
    endif.

    if WA_BSAK-SHKZG = 'H'.
      if WA_SAIDA_0100-VL_FIS gt 0.
        WA_SAIDA_0100-REC_DES     = 'Receita'.
      else.
        WA_SAIDA_0100-REC_DES     = 'Despesa'.
      endif.
    endif.


    if WA_BSAK-SHKZG = 'S'.
      if WA_SAIDA_0100-VL_FIS gt 0.
        WA_SAIDA_0100-REC_DES     = 'Despesa'.
      else.
        WA_SAIDA_0100-REC_DES     = 'Receita'.
      endif.
    endif.

    " Dif. VC Cont. x VC. Fiscal
    WA_SAIDA_0100-DIF_CTB_FIS = WA_SAIDA_0100-VL_CTB - WA_SAIDA_0100-VL_FIS.

    "VC Cx doc Contábil
    WA_SAIDA_0100-VL_CTB_CX   = WA_ZFIT0132-VR_CORRECAO_REAL.

    "VC CTB-VC DCTO Contábil
    WA_SAIDA_0100-VL_CTB_DOC  = WA_SAIDA_0100-VL_CTB + WA_SAIDA_0100-VL_CTB_CX.

    "VC competencia
    WA_SAIDA_0100-VL_CTB_CP   = 0.

    WA_SAIDA_0100-SGTXT       = WA_BSAK-SGTXT.
    WA_SAIDA_0100-ORIGEM      = 'SIGAM'.

    append WA_SAIDA_0100 to IT_SAIDA_0100.

    " para juros e correcao valor 0
    clear WA_SAIDA_0100-VL_CTB_DOC.
    "juros
    if WA_ZFIT0132-VR_JUROS_REAL ne 0.
      multiply  WA_ZFIT0132-VR_JUROS_REAL  by -1.
      multiply  WA_ZFIT0132-VR_JUROS_DOLAR by -1.

      WA_SAIDA_0100-BELNR       = WA_ZFIT0132-DOCUMENTO_SAP_JUROS.
      WA_SAIDA_0100-DMBTR       = WA_ZFIT0132-VR_JUROS_REAL.
      WA_SAIDA_0100-DMBE2       = WA_ZFIT0132-VR_JUROS_DOLAR.
      WA_SAIDA_0100-PTAXCA      = WA_ZFIT0132-VR_JUROS_REAL / WA_ZFIT0132-VR_JUROS_DOLAR.
      WA_SAIDA_0100-PTAXCO      = WA_ZFIT0132-VR_JUROS_REAL / WA_ZFIT0132-VR_JUROS_DOLAR.
      "Cálculo  VC Contábil
      WA_SAIDA_0100-VL_CTB      = ( WA_SAIDA_0100-PTAXCO - WA_SAIDA_0100-PTAXCA ) * WA_SAIDA_0100-DMBE2.

      WA_SAIDA_0100-BUDAT = WA_ZFIT0132-DATA_BAIXA.
      "Cálculo VC Fiscal
      if WA_SAIDA_0100-BUDAT le WA_ZFIT0130-DT_TX_FIXADA.
        WA_SAIDA_0100-VL_FIS      = ( WA_SAIDA_0100-PTAXCO - WA_ZFIT0130-TX_FIXADA ) *  WA_SAIDA_0100-DMBE2.
      else.
        WA_SAIDA_0100-VL_FIS      = ( WA_SAIDA_0100-PTAXCO - WA_SAIDA_0100-PTAXCA ) *  WA_SAIDA_0100-DMBE2.
      endif.

      if WA_BSAK-SHKZG = 'H'.
        if WA_SAIDA_0100-VL_FIS gt 0.
          WA_SAIDA_0100-REC_DES     = 'Receita'.
        else.
          WA_SAIDA_0100-REC_DES     = 'Despesa'.
        endif.
      endif.


      if WA_BSAK-SHKZG = 'S'.
        if WA_SAIDA_0100-VL_FIS gt 0.
          WA_SAIDA_0100-REC_DES     = 'Despesa'.
        else.
          WA_SAIDA_0100-REC_DES     = 'Receita'.
        endif.
      endif.

      "VC Cx doc Contábil
      WA_SAIDA_0100-VL_CTB_CX   = 0.

      " Dif. VC Cont. x VC. Fiscal
      WA_SAIDA_0100-DIF_CTB_FIS = WA_SAIDA_0100-VL_CTB - WA_SAIDA_0100-VL_FIS.
      WA_SAIDA_0100-VL_CTB_DOC  = WA_SAIDA_0100-VL_CTB + WA_SAIDA_0100-VL_CTB_CX.

      WA_SAIDA_0100-ORIGEM      = 'SIGAM'.
      append WA_SAIDA_0100 to IT_SAIDA_0100.
    endif.


    "correção
    if WA_ZFIT0132-VR_CORRECAO_REAL ne 0.
      multiply  WA_ZFIT0132-VR_CORRECAO_REAL  by -1.
      WA_SAIDA_0100-BELNR       = WA_ZFIT0132-DOCUMENTO_SAP_CORRECAO.
      WA_SAIDA_0100-DMBTR       = WA_ZFIT0132-VR_CORRECAO_REAL.
      WA_SAIDA_0100-DMBE2       = 0.
      WA_SAIDA_0100-PTAXCA      = 0.
      WA_SAIDA_0100-PTAXCO      = 0.
      "Cálculo  VC Contábil
      WA_SAIDA_0100-VL_CTB      = ( WA_SAIDA_0100-PTAXCO - WA_SAIDA_0100-PTAXCA ) * WA_SAIDA_0100-DMBE2.

      WA_SAIDA_0100-BUDAT = WA_ZFIT0132-DATA_BAIXA.
      "Cálculo VC Fiscal
      if WA_SAIDA_0100-BUDAT le WA_ZFIT0130-DT_TX_FIXADA.
        WA_SAIDA_0100-VL_FIS      = ( WA_SAIDA_0100-PTAXCO - WA_ZFIT0130-TX_FIXADA ) *  WA_SAIDA_0100-DMBE2.
      else.
        WA_SAIDA_0100-VL_FIS      = ( WA_SAIDA_0100-PTAXCO - WA_SAIDA_0100-PTAXCA ) *  WA_SAIDA_0100-DMBE2.
      endif.

      if WA_BSAK-SHKZG = 'H'.
        if WA_SAIDA_0100-VL_FIS gt 0.
          WA_SAIDA_0100-REC_DES     = 'Receita'.
        else.
          WA_SAIDA_0100-REC_DES     = 'Despesa'.
        endif.
      endif.


      if WA_BSAK-SHKZG = 'S'.
        if WA_SAIDA_0100-VL_FIS gt 0.
          WA_SAIDA_0100-REC_DES     = 'Despesa'.
        else.
          WA_SAIDA_0100-REC_DES     = 'Receita'.
        endif.
      endif.

      "VC Cx doc Contábil
      WA_SAIDA_0100-VL_CTB_CX   = 0.

      " Dif. VC Cont. x VC. Fiscal
      WA_SAIDA_0100-DIF_CTB_FIS = WA_SAIDA_0100-VL_CTB - WA_SAIDA_0100-VL_FIS.
      WA_SAIDA_0100-VL_CTB_DOC  = WA_SAIDA_0100-VL_CTB + WA_SAIDA_0100-VL_CTB_CX.


      WA_SAIDA_0100-ORIGEM      = 'SIGAM'.
      append WA_SAIDA_0100 to IT_SAIDA_0100.
    endif.

  endloop.

  "SAP
  loop at IT_BSAD into WA_BSAD.
    clear WA_SAIDA_0100.
    if P_HKONT is not initial.
      if WA_BSAD-HKONT not in P_HKONT.
        continue.
      endif.
    endif.
    read table IT_BKPF into WA_BKPF with key BUKRS = WA_BSAD-BUKRS
                                         BELNR = WA_BSAD-BELNR
                                         GJAHR = WA_BSAD-GJAHR binary search.
    if WA_BKPF-STBLG is not initial.
      select single BUDAT
        into @data(VBUDAT)
        from BKPF
        where BUKRS = @WA_BKPF-BUKRS
        and   BELNR = @WA_BKPF-STBLG
        and   GJAHR = @WA_BKPF-STJAH.
      if VBUDAT+0(6) = WA_BKPF-BUDAT+0(6).
        continue.
      endif.
    endif.
    WA_SAIDA_0100-BELNR       = WA_BSAD-BELNR.
    WA_SAIDA_0100-HKONT       = WA_BSAD-HKONT.



    if WA_BSAD-SHKZG = 'S'.
      multiply WA_BSAD-WRBTR by -1.
      multiply WA_BSAD-DMBTR by -1.
      multiply WA_BSAD-DMBE2 by -1.
    endif.

    read table IT_SKB1 into WA_SKB1 with key SAKNR = WA_BSAD-HKONT binary search.

    read table IT_SKAT into WA_SKAT with key SAKNR = WA_BSAD-HKONT binary search.
    WA_SAIDA_0100-TXT50       = WA_SKAT-TXT50.

    WA_SAIDA_0100-GSBER       = WA_BSAD-GSBER.
    WA_SAIDA_0100-BUDAT       = WA_BSAD-BUDAT.
    WA_SAIDA_0100-ANOMEL      = WA_BSAD-BUDAT+0(6).
    concatenate WA_SAIDA_0100-ANOMEL+0(4) '.' WA_SAIDA_0100-ANOMEL+4(2) into WA_SAIDA_0100-ANOMEL.
    WA_SAIDA_0100-DMBTR       = WA_BSAD-DMBTR.
    WA_SAIDA_0100-DMBE2       = WA_BSAD-DMBE2.
    WA_SAIDA_0100-WRBTR       = WA_BSAD-WRBTR.
    WA_SAIDA_0100-WAERS       = WA_BSAD-WAERS.
    WA_SAIDA_0100-AUGBL       = WA_BSAD-AUGBL.
    WA_SAIDA_0100-AUGDT       = WA_BSAD-AUGDT.
    WA_SAIDA_0100-ANOMEC      = WA_BSAD-AUGDT+0(6).
    concatenate WA_SAIDA_0100-ANOMEC+0(4) '.' WA_SAIDA_0100-ANOMEC+4(2) into WA_SAIDA_0100-ANOMEC.

    "substitui se compensação foi estornada
    WA_SAIDA_0100-VL_CTB_CX   = 0.
    read table IT_BSE_ESTD into WA_BSE_ESTD with key BUKRS_CLR = WA_BSAD-BUKRS
                                                     BELNR     = WA_BSAD-BELNR
                                                     GJAHR     = WA_BSAD-GJAHR binary search.
    if SY-SUBRC = 0 and WA_BSAD-STBLG is not initial.
      WA_SAIDA_0100-AUGBL       = WA_BSE_ESTD-BELNR_CLR.
      clear WA_BSAD_EST.
      read table IT_BSAD_ESTD into WA_BSAD_ESTD with key BUKRS = WA_BSE_ESTD-BUKRS_CLR
                                                         BELNR = WA_BSE_ESTD-BELNR_CLR
                                                         GJAHR = WA_BSE_ESTD-GJAHR_CLR binary search.
      if SY-SUBRC = 0.
        WA_SAIDA_0100-AUGDT       = WA_BSAD_ESTD-AUGDT.
        WA_SAIDA_0100-ANOMEC      = WA_BSAD-AUGDT+0(6).
        concatenate WA_SAIDA_0100-ANOMEC+0(4) '.' WA_SAIDA_0100-ANOMEC+4(2) into WA_SAIDA_0100-ANOMEC.
      endif.
      "VC Cx doc Contábil
      read table IT_BSE_ESTD into WA_BSE with key   BUKRS_CLR = WA_BSAD_ESTD-BUKRS
                                                    BELNR_CLR = WA_BSAD_ESTD-BELNR
                                                    GJAHR_CLR = WA_BSAD_ESTD-GJAHR
                                                    BELNR     = WA_BSAD-BELNR
                                                    GJAHR     = WA_BSAD-GJAHR
                                                    BUZEI     = WA_BSAD-BUZEI. " BINARY SEARCH.

      if SY-SUBRC = 0.
        multiply WA_BSE-RDIFF by -1.
        WA_SAIDA_0100-VL_CTB_CX   = WA_BSE-RDIFF.
        "Inverte sinal
        multiply WA_BSAD-WRBTR by -1.
        multiply WA_BSAD-DMBTR by -1.
        multiply WA_BSAD-DMBE2 by -1.
        "
        multiply WA_SAIDA_0100-WRBTR by -1.
        multiply WA_SAIDA_0100-DMBTR by -1.
        multiply WA_SAIDA_0100-DMBE2 by -1.
      else.
        WA_SAIDA_0100-VL_CTB_CX   = 0.
      endif.
    else.
      read table IT_BSE_EST into WA_BSE_EST with key BUKRS_CLR = WA_BSAD-BUKRS
                                                     BELNR     = WA_BSAD-BELNR
                                                     GJAHR     = WA_BSAD-GJAHR binary search.
      if SY-SUBRC = 0.
        WA_SAIDA_0100-AUGBL       = WA_BSE_EST-BELNR_CLR.
        clear WA_BSAD_EST.
        read table IT_BSAD_EST into WA_BSAD_EST with key BUKRS = WA_BSE_EST-BUKRS_CLR
                                                         BELNR = WA_BSE_EST-BELNR_CLR
                                                         GJAHR = WA_BSE_EST-GJAHR_CLR binary search.
        if SY-SUBRC = 0.
          WA_SAIDA_0100-AUGDT       = WA_BSAD_EST-BUDAT.
          WA_SAIDA_0100-ANOMEC      = WA_BSAD-BUDAT+0(6).
          concatenate WA_SAIDA_0100-ANOMEC+0(4) '.' WA_SAIDA_0100-ANOMEC+4(2) into WA_SAIDA_0100-ANOMEC.
        endif.
        "VC Cx doc Contábil
        read table IT_BSE_EST into WA_BSE with key  BUKRS_CLR = WA_BSAD_EST-BUKRS
                                                    BELNR_CLR = WA_BSAD_EST-BELNR
                                                    GJAHR_CLR = WA_BSAD_EST-GJAHR
                                                    BELNR     = WA_BSAD-BELNR
                                                    GJAHR     = WA_BSAD-GJAHR
                                                    BUZEI     = WA_BSAD-BUZEI. " BINARY SEARCH.

        if SY-SUBRC = 0.
          if WA_BSAD-SHKZG = 'S'.
            multiply WA_BSE-RDIFF by -1.
          endif.
          WA_SAIDA_0100-VL_CTB_CX   = WA_BSE-RDIFF.
        else.
          WA_SAIDA_0100-VL_CTB_CX   = 0.
        endif.
      else.
        "VC Cx doc Contábil
        read table IT_BSE into WA_BSE with key  BUKRS_CLR = WA_BSAD-BUKRS
                                                BELNR_CLR = WA_BSAD-AUGBL
                                                GJAHR_CLR = WA_BSAD-AUGGJ
                                                BELNR     = WA_BSAD-BELNR
                                                GJAHR     = WA_BSAD-GJAHR
                                                BUZEI     = WA_BSAD-BUZEI binary search.

        if SY-SUBRC = 0.
          if WA_BSAD-SHKZG = 'S'.
            WA_SAIDA_0100-VL_CTB_CX   = WA_BSE-RDIFF * -1.
          else.
            WA_SAIDA_0100-VL_CTB_CX   = WA_BSE-RDIFF.
          endif.
        else.
          WA_SAIDA_0100-VL_CTB_CX   = 0.
        endif.
      endif.
    endif.

    if WA_SAIDA_0100-AUGBL is not initial.
      refresh: IT_BKPF_, IT_PARTIDAS.
      WA_BKPF_-BUKRS = P_BUKRS.
      WA_BKPF_-BELNR = WA_SAIDA_0100-AUGBL.
      WA_BKPF_-GJAHR = WA_SAIDA_0100-AUGDT+0(4).
      append WA_BKPF_ to IT_BKPF_.

      call function 'Z_FI_GL_PARTIDAS'
        exporting
          I_CONTAS    = IT_CONTAS
        tables
          IT_BKPF     = IT_BKPF_
          IT_PARTIDAS = IT_PARTIDAS.
      if IT_PARTIDAS[] is not initial.
        W_LINES =  LINES( IT_PARTIDAS ).
        if  W_LINES = 1.
          read table IT_PARTIDAS index 1.
          WA_SAIDA_0100-CONTA_VC    = IT_PARTIDAS-HKONT.
        else.
          select single *
            from T030H
            into WA_T030H
            where KTOPL = '0050'
            and   HKONT = WA_SAIDA_0100-HKONT.
          loop at IT_PARTIDAS.
            if IT_PARTIDAS-HKONT = WA_T030H-LSREA.
              WA_SAIDA_0100-CONTA_VC    = IT_PARTIDAS-HKONT.
            endif.
          endloop.
        endif.
      endif.
    endif.

    read table IT_BKPF into WA_BKPF with key BUKRS = WA_BSAD-BUKRS
                                             BELNR = WA_BSAD-BELNR
                                             GJAHR = WA_BSAD-GJAHR binary search.

    if WA_BSAD-WAERS = 'USD'.
      WA_SAIDA_0100-PTAXCA = WA_BSAD-DMBTR / WA_BSAD-DMBE2.
    else.
      WA_SAIDA_0100-PTAXCA = WA_BSAD-DMBTR / WA_BSAD-WRBTR.
    endif.

*    READ TABLE IT_BKPF INTO WA_BKPF WITH KEY BUKRS = WA_BSAD-BUKRS
*                                             BELNR = WA_BSAD-AUGBL
*                                             GJAHR = WA_BSAD-AUGGJ BINARY SEARCH.
    " Busca TAXA de compensação / doc. estornado de compensação
    select single *
      from BKPF
      into corresponding fields of WA_BKPF
      where BUKRS = WA_BSAD-BUKRS
      and   BELNR = WA_SAIDA_0100-AUGBL
      and   GJAHR = WA_SAIDA_0100-AUGDT+0(4).
    if  WA_BKPF-KURSF ne 0.
      WA_SAIDA_0100-PTAXCO      = WA_BKPF-KURSF.
    else.
      if WA_BKPF-KURS2 lt 0.
        WA_SAIDA_0100-PTAXCO      = WA_BKPF-KURS2 * -1.
      else.
        WA_SAIDA_0100-PTAXCO      = WA_BKPF-KURS2.
      endif.
    endif.

    "Cálculo  VC Contábil
    if WA_BSAD-WAERS = 'USD'.
      WA_SAIDA_0100-VL_CTB      = ( WA_SAIDA_0100-PTAXCO - WA_SAIDA_0100-PTAXCA ) * WA_BSAD-DMBE2.
    else.
      WA_SAIDA_0100-VL_CTB      = ( WA_SAIDA_0100-PTAXCO - WA_SAIDA_0100-PTAXCA ) * WA_BSAD-WRBTR.
    endif.

    "Cálculo VC Fiscal
    if WA_BSAD-WAERS = 'USD'.
      if WA_BSAD-BUDAT le WA_ZFIT0130-DT_TX_FIXADA.
        WA_SAIDA_0100-VL_FIS      = ( WA_SAIDA_0100-PTAXCO - WA_ZFIT0130-TX_FIXADA ) * WA_BSAD-DMBE2.
      else.
        WA_SAIDA_0100-VL_FIS      = ( WA_SAIDA_0100-PTAXCO - WA_SAIDA_0100-PTAXCA ) * WA_BSAD-DMBE2.
      endif.
    else.
      if WA_BSAD-BUDAT le WA_ZFIT0130-DT_TX_FIXADA.
        WA_SAIDA_0100-VL_FIS      = ( WA_SAIDA_0100-PTAXCO - WA_ZFIT0130-TX_FIXADA ) * WA_BSAD-WRBTR.
      else.
        WA_SAIDA_0100-VL_FIS      = ( WA_SAIDA_0100-PTAXCO - WA_SAIDA_0100-PTAXCA ) * WA_BSAD-WRBTR.
      endif.
    endif.

*    IF WA_BSAD-SHKZG = 'H'.
*      IF WA_SAIDA_0100-VL_FIS GT 0.
*        WA_SAIDA_0100-REC_DES     = 'Receita'.
*      ELSE.
*        WA_SAIDA_0100-REC_DES     = 'Despesa'.
*      ENDIF.
*    ENDIF.

    if WA_BSAD-SHKZG = 'H'.
      if WA_SAIDA_0100-VL_FIS gt 0.
        WA_SAIDA_0100-REC_DES     = 'Despesa'.
      else.
        WA_SAIDA_0100-REC_DES     = 'Receita'.
      endif.
    endif.

    if WA_BSAD-SHKZG = 'S'.
      if WA_SAIDA_0100-VL_FIS gt 0.
        WA_SAIDA_0100-REC_DES     = 'Despesa'.
      else.
        WA_SAIDA_0100-REC_DES     = 'Receita'.
      endif.
    endif.

    if P_BELNR is not initial.
      if WA_SAIDA_0100-AUGBL not in P_BELNR.
        continue.
      endif.
    endif.

    " Dif. VC Cont. x VC. Fiscal
    WA_SAIDA_0100-DIF_CTB_FIS = WA_SAIDA_0100-VL_CTB - WA_SAIDA_0100-VL_FIS.


    "VC CTB-VC DCTO Contábil
    WA_SAIDA_0100-VL_CTB_DOC  = ( WA_SAIDA_0100-VL_CTB ) - ( WA_SAIDA_0100-VL_CTB_CX ).

    "VC competencia
    clear WA_ZGL012.
    read table IT_ZGL012 into WA_ZGL012  with key BUKRS = WA_BSAD-BUKRS
                                                  BELNR = WA_BSAD-BELNR
                                                  BUZEI = WA_BSAD-BUZEI binary search.
    WA_SAIDA_0100-VL_CTB_CP   = WA_ZGL012-VLR_ACUM_MES_ATU.

    WA_SAIDA_0100-SGTXT       = WA_BSAD-SGTXT.
    WA_SAIDA_0100-ORIGEM      = 'SAP'.

    append WA_SAIDA_0100 to IT_SAIDA_0100.
  endloop.

endform.

form F_IMPRIMIR_DADOS.
  perform F_MONTAR_LAYOUT.

  call screen 0100.

endform.

form F_MONTAR_LAYOUT .

  refresh T_FIELDCATALOG.
  perform F_MONTAR_ESTRUTURA using:
        1   'BSAD'     'BELNR'       'IT_SAIDA_0100'  'BELNR'       'Nº documento'         '10' ' ' ' ' 'X',
        1   'BSAD'     'HKONT'       'IT_SAIDA_0100'  'HKONT'       'Conta Patrimonial'    '10' ' ' ' ' 'X',
        1   ''         ''            'IT_SAIDA_0100'  'TXT50'       'Denominação  conta'   '30' ' ' ' ' 'X',
        1   'BSAD'     'GSBER'       'IT_SAIDA_0100'  'GSBER'       'Divisão'              '10' ' ' ' ' 'X',
        1   'BSAD'     'BUDAT'       'IT_SAIDA_0100'  'BUDAT'       'Data do Lanc'         '12' ' ' ' ' 'X',
        1   ''          ''           'IT_SAIDA_0100'  'ANOMEL'      'Ano/Mes lct'          '12' ' ' ' ' 'X',
        1   'BSAD'     'DMBTR'       'IT_SAIDA_0100'  'DMBTR'       'Montante Interna'     '16' ' ' ' ' 'X',
        1   'BSAD'     'DMBE2'       'IT_SAIDA_0100'  'DMBE2'       'Montante Forte'       '16' ' ' ' ' 'X',
        1   'BSAD'     'WRBTR'       'IT_SAIDA_0100'  'WRBTR'       'Montante Dcto'        '18' ' ' ' ' 'X',
        1   'BSAD'     'WAERS'       'IT_SAIDA_0100'  'WAERS'       'Moeda Dcto'           '12' ' ' ' ' 'X',
        1   'BSAD'     'AUGBL'       'IT_SAIDA_0100'  'AUGBL'       'Doc. Compensação'     '10' ' ' ' ' 'X',
        1   'BSAD'     'AUGDT'       'IT_SAIDA_0100'  'AUGDT'       'Data Compensação'     '10' ' ' ' ' 'X',
        1   ''          ''           'IT_SAIDA_0100'  'ANOMEC'      'Ano/Mes comp'         '12' ' ' ' ' 'X',
        1   'BKPF'     'KURSF'       'IT_SAIDA_0100'  'PTAXCA'      'Ptax Captação'        '12' ' ' ' ' 'X',
        1   'BKPF'     'KURSF'       'IT_SAIDA_0100'  'PTAXCO'      'Ptax Compensação'     '12' ' ' ' ' 'X',
        1   'BSAD'     'DMBTR'       'IT_SAIDA_0100'  'VL_CTB'      'VC Contábil'          '15' ' ' ' ' 'X',
        1   'BSAD'     'DMBTR'       'IT_SAIDA_0100'  'VL_FIS'      'VC Fiscal'            '15' ' ' ' ' 'X',
        1   ''          ''           'IT_SAIDA_0100'  'REC_DES'     'Receita/Despesa'      '15' ' ' ' ' 'X',
        1   'BSAD'     'DMBTR'       'IT_SAIDA_0100'  'DIF_CTB_FIS' 'VC Ctb-VC Fiscal'     '15' ' ' ' ' 'X',
        1   'BSAD'     'DMBTR'       'IT_SAIDA_0100'  'VL_CTB_CX'   'VC Dcto Contábil'     '15' ' ' ' ' 'X',
        1   'BSAD'     'DMBTR'       'IT_SAIDA_0100'  'VL_CTB_DOC'  'VC Ctb - VC Dcto'     '15' ' ' ' ' 'X',
        1   'BSAD'     'DMBTR'       'IT_SAIDA_0100'  'VL_CTB_CP'   'VC Competência'       '15' ' ' ' ' 'X',
        1   ''          ''           'IT_SAIDA_0100'  'SGTXT'       'Texto'                '25' ' ' ' ' 'X',
        1   ''          ''           'IT_SAIDA_0100'  'CONTA_VC'    'Conta VC'             '10' ' ' ' ' 'X',
        1   ''          ''           'IT_SAIDA_0100'  'ORIGEM'      'Origem'               '10' ' ' ' ' 'X'.


endform.                    " MONTAR_LAYOUT

*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SPACE  text
*----------------------------------------------------------------------*
form F_MONTAR_LAYOUT2   using    P_EDIT.
  refresh T_FIELDCATALOG.

  perform F_MONTAR_ESTRUTURA using:
        0   ''          ''               'IT_SAIDA_0200'  'LINHA'         'Linha'               '06' ' '    ' ' '',
        1   'SKA1'      'SAKNR'          'IT_SAIDA_0200'  'HKONT'         'Conta'               '10' P_EDIT ' ' 'X',
        2   'ZFIT0131'  'ID_SISTEMA'     'IT_SAIDA_0200'  'ID_SISTEMA'    'ID_Sistema'          '10' P_EDIT ' ' 'X',
        3   ''          ''               'IT_SAIDA_0200'  'TXT50'         'Descrição'           '30' ' '    ' ' '',
        4   ''          ''               'IT_SAIDA_0200'  'USUARIO'       'Usuário'             '20' ' '    ' ' '',
        5   'ZFIT0131'  'DATA_REGISTRO'  'IT_SAIDA_0200'  'DATA_REGISTRO' 'Data'                '10' ' '    ' ' '',
        6   'ZFIT0131'  'HORA_REGISTRO'  'IT_SAIDA_0200'  'HORA_REGISTRO' 'Hora'                '10' ' '    ' ' ''.
endform.

form F_MONTAR_LAYOUT3   using    P_EDIT.
  refresh T_FIELDCATALOG.

  perform F_MONTAR_ESTRUTURA using:
        0   ''         ''             'IT_SAIDA_0300'  'LINHA'         'Linha'               '06' ' '    ' ' '',
        1   ''         ''             'IT_SAIDA_0300'  'STATUS'        'Status'              '08' ' '    ' ' '',
        1   'ZFIT0130' 'BUKRS'        'IT_SAIDA_0300'  'BUKRS'         'Empresa'             '08' P_EDIT ' ' 'X',
        2   ''         ''             'IT_SAIDA_0300'  'GJAHR'         'Exercicio'           '06' P_EDIT ' ' 'X',
        3   'ZFIT0130' 'DT_REGIME'    'IT_SAIDA_0300'  'DT_REGIME'     'Dt. Mud.Regime'      '15' P_EDIT ' ' 'X',
        4   'ZFIT0130' 'REGIME'       'IT_SAIDA_0300'  'REGIME'        'Regime'              '08' P_EDIT ' ' 'X',
        5   'ZFIT0130' 'TX_FIXADA'    'IT_SAIDA_0300'  'TX_FIXADA'     'Tx. Câmbio'          '10' P_EDIT ' ' 'X',
        6   'ZFIT0130' 'DT_REGIME'    'IT_SAIDA_0300'  'DT_TX_FIXADA'  'Dt. Fixada'          '10' P_EDIT ' ' 'X',
        6   ''         ''             'IT_SAIDA_0300'  'OBSERV'        'Observação'          '30' P_EDIT ' ' 'X',
        7   ''         ''             'IT_SAIDA_0300'  'USUARIO'       'Usuário'             '20' ' '    ' ' '',
        8   'ZFIT0130' 'DATA_REGISTRO' 'IT_SAIDA_0300'  'DATA_REGISTRO' 'Data'                '10' ' '    ' ' '',
        9   'ZFIT0130' 'HORA_REGISTRO' 'IT_SAIDA_0300'  'HORA_REGISTRO' 'Hora'                '10' ' '    ' ' ''.
endform.

form F_MONTAR_LAYOUT4   using    P_EDIT.
  refresh T_FIELDCATALOG.

  perform F_MONTAR_ESTRUTURA using:
        0   ''         ''             'IT_SAIDA_0400'  'LINHA'            'Linha'            '06' ' '    ' ' '',
        1   ''         ''             'IT_SAIDA_0400'  'STATUS'           'Status'           '08' ' '    ' ' '',
        2   ''         ''             'IT_SAIDA_0400'  'CONTRATO_NUMERO'  'Contrato'         '08' ' '    ' ' '',
        3   'ZFIT0135' 'DATA_EFETIVA' 'IT_SAIDA_0400'  'DATA_EFETIVA'     'Data Efetiva'     '10' ' '    ' ' '',
        4   'ZFIT0135' 'ENT_SAI'      'IT_SAIDA_0400'  'ENT_SAI'          'E/S'              '06' ' '    ' ' '',
        5   'ZFIT0135' 'BUKRS'        'IT_SAIDA_0400'  'BUKRS'            'Empresa'          '08' P_EDIT ' ' 'X',
        6   'ZFIT0135' 'BELNR'        'IT_SAIDA_0400'  'BELNR'            'Documento'        '10' P_EDIT ' ' 'X',
        7   'ZFIT0135' 'GJAHR'        'IT_SAIDA_0400'  'GJAHR'            'Exercicio'        '06' P_EDIT ' ' 'X',
        8   'ZFIT0135' 'VALOR_VALIDO' 'IT_SAIDA_0400'  'VALOR_VALIDO'     'Valor Válido'     '15' ' '    ' ' '',
        9   'ZFIT0135' 'VALOR_PAGO'   'IT_SAIDA_0400'  'VALOR_PAGO'       'Valor Pago'       '15' ' '    ' ' '',
       10   'ZFIT0135' 'TAXA'         'IT_SAIDA_0400'  'TAXA'             'Taxa'             '12' ' '    ' ' '',
       11   ''         ''             'IT_SAIDA_0400'  'USUARIO'          'Usuário'          '20' ' '    ' ' '',
       12   'ZFIT0135' 'DATA_REGISTRO'    'IT_SAIDA_0400'  'DATA_REGISTRO'    'Data'             '10' ' '    ' ' '',
       13   'ZFIT0135' 'HORA_REGISTRO' 'IT_SAIDA_0400' 'HORA_REGISTRO'   'Hora'              '10' ' '    ' ' ''.
endform.
*------------------------------------------------------------------*
*&      Form  F_MONTAR_ESTRUTURA
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
form F_MONTAR_ESTRUTURA  using  P_COL_POS   P_REF_TABNAME P_REF_FIELDNAME P_TABNAME P_FIELD
                                P_SCRTEXT_L P_OUTPUTLEN   P_EDIT          P_SUM     P_EMPHASIZE.

  clear W_FIELDCATALOG.
  W_FIELDCATALOG-FIELDNAME     = P_FIELD.
  W_FIELDCATALOG-TABNAME       = P_TABNAME.
  W_FIELDCATALOG-REF_TABLE     = P_REF_TABNAME.
  W_FIELDCATALOG-REF_FIELD     = P_REF_FIELDNAME.

  W_FIELDCATALOG-KEY           = ' '.


  W_FIELDCATALOG-EDIT          = P_EDIT.
  W_FIELDCATALOG-DO_SUM        = P_SUM.

  W_FIELDCATALOG-COL_POS       = P_COL_POS.

  if P_FIELD = 'STATUS'.
    W_FIELDCATALOG-ICON          = 'X'.
    W_FIELDCATALOG-HOTSPOT       = 'X'.
  endif.


  if  P_FIELD = 'AUGBL'.
    W_FIELDCATALOG-HOTSPOT       = 'X'.
  endif.

  if P_OUTPUTLEN is not initial.
    W_FIELDCATALOG-OUTPUTLEN   = P_OUTPUTLEN.
  endif.

  W_FIELDCATALOG-NO_OUT        = ' '.
  W_FIELDCATALOG-REPTEXT       = P_SCRTEXT_L.
  W_FIELDCATALOG-SCRTEXT_S     = P_SCRTEXT_L.
  W_FIELDCATALOG-SCRTEXT_M     = P_SCRTEXT_L.
  W_FIELDCATALOG-SCRTEXT_L     = P_SCRTEXT_L.
  W_FIELDCATALOG-EMPHASIZE     = P_EMPHASIZE.

  append W_FIELDCATALOG to T_FIELDCATALOG.
endform.                    " F_MONTAR_ESTRUTURA
*&---------------------------------------------------------------------*
*&      Form  F_BUSCA_DADOS2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form F_BUSCA_DADOS2 .
  data: WL_SKAT     type SKAT,
        TABIX       type SY-TABIX,
        VSGTXT      type BSEG-SGTXT,
        VSGTXT_1(6),
        VSGTXT_2(6),
        VCREDE(1),
        VAPROP(1).

  if WG_ACAO = C_ADD. "Novo Lançamento
    WG_ACAO = C_MODIF.
    if D_BUTT1 = 'BTN1'.
      select *
       from ZFIT0130
       into corresponding fields of table IT_SAIDA_0300
       where BUKRS = P_BUKRS.
    elseif D_BUTT1 = 'BTN2'.
      select *
        from ZFIT0131
           into corresponding fields of table IT_SAIDA_0200.
    elseif D_BUTT1 = 'BTN3'.
      select *
       from ZFIT0135
       into corresponding fields of table IT_SAIDA_0400
       where BUKRS = P_BUKRS.
      select *
       from ZFIT0134
       appending corresponding fields of table IT_SAIDA_0400
       where ENT_SAI = 'E'
       and   not exists ( select * from ZFIT0135 where CONTRATO_NUMERO = ZFIT0134~CONTRATO_NUMERO
                                                 and   DATA_EFETIVA    = ZFIT0134~DATA_EFETIVA
                                                 and   ENT_SAI         in ( ' ' , 'E' ) ).
      select *
      from ZFIT0134
      appending corresponding fields of table IT_SAIDA_0400
      where ENT_SAI = 'S'
      and   not exists ( select * from ZFIT0135 where CONTRATO_NUMERO = ZFIT0134~CONTRATO_NUMERO
                                                and   DATA_EFETIVA    = ZFIT0134~DATA_EFETIVA
                                                and   ENT_SAI         = 'S' ).
      loop at IT_SAIDA_0400 into WA_SAIDA_0400.
        TABIX = SY-TABIX.
        if WA_SAIDA_0400-BELNR is initial.
          if WA_SAIDA_0400-ENT_SAI = 'E'.
            VCREDE = 'C'.
            VAPROP = 'C'.
          else.
            VCREDE = 'D'.
            VAPROP = 'C'.
          endif.

          select  *
            from ZFIT0133
            into @data(W133)
            where CONTRATO_NUMERO   eq @WA_SAIDA_0400-CONTRATO_NUMERO
            and   DATA_BAIXA        eq @WA_SAIDA_0400-DATA_EFETIVA
            and   VALOR_ORIGINAL_D  eq @WA_SAIDA_0400-VALOR_VALIDO
            and   TIPO              eq 'PRI'
            and   CREDITO_DEBITO    eq @VCREDE
            and   APROPRIACAO_CAIXA eq @VAPROP.

            concatenate W133-CHAVE_REFERENCIA '%' into W133-CHAVE_REFERENCIA.
            "
            select  OBJ_KEY BUKRS BELNR GJAHR
                 from ZIB_CONTABIL_CHV
                 into   WA_ZIB_CONTABIL_CHV
                 where OBJ_KEY like W133-CHAVE_REFERENCIA.
              WA_ZIB_CONTABIL_CHV-XREF1 =  W133-PAR_CONTABIL.
              select single   *
                from BSAK
                into @data(WBSAK)
                 where BUKRS    eq @WA_ZIB_CONTABIL_CHV-BUKRS
                 and   BELNR    eq @WA_ZIB_CONTABIL_CHV-BELNR
                 and   GJAHR    eq @WA_ZIB_CONTABIL_CHV-GJAHR
                 and   XREF1    eq @WA_ZIB_CONTABIL_CHV-XREF1.

              if SY-SUBRC = 0.
                WA_SAIDA_0400-BUKRS = WA_ZIB_CONTABIL_CHV-BUKRS.
                WA_SAIDA_0400-BELNR = WA_ZIB_CONTABIL_CHV-BELNR.
                WA_SAIDA_0400-GJAHR = WA_ZIB_CONTABIL_CHV-GJAHR.
                WA_SAIDA_0400-STATUS = ICON_MESSAGE_WARNING_SMALL.
                modify IT_SAIDA_0400 from WA_SAIDA_0400 index TABIX transporting BUKRS BELNR GJAHR STATUS.
              else.
                select single   *
                    from BSIK
                    into @data(WBSIK)
                     where BUKRS    eq @WA_ZIB_CONTABIL_CHV-BUKRS
                     and   BELNR    eq @WA_ZIB_CONTABIL_CHV-BELNR
                     and   GJAHR    eq @WA_ZIB_CONTABIL_CHV-GJAHR
                     and   XREF1    eq @WA_ZIB_CONTABIL_CHV-XREF1.
                if SY-SUBRC = 0.
                  WA_SAIDA_0400-BUKRS = WA_ZIB_CONTABIL_CHV-BUKRS.
                  WA_SAIDA_0400-BELNR = WA_ZIB_CONTABIL_CHV-BELNR.
                  WA_SAIDA_0400-GJAHR = WA_ZIB_CONTABIL_CHV-GJAHR.
                  WA_SAIDA_0400-STATUS = ICON_MESSAGE_WARNING_SMALL.
                  modify IT_SAIDA_0400 from WA_SAIDA_0400 index TABIX transporting BUKRS BELNR GJAHR STATUS.
                endif.
              endif.
            endselect.
          endselect.
        endif.
      endloop.
    elseif D_BUTT1 = 'BTN4'.
      select *
        from BKPF
        into table @data(TBKPF)
        where BUKRS eq @P_BUKRS
        and   BLART eq 'XR'
        and   BUDAT in @P_AUGDT
        and   AWTYP eq 'IDOC'
        and   GJAHR eq @P_AUGDT-LOW+0(4)
        and   WAERS ne 'BRL'
        and   STBLG eq ''.

      loop at TBKPF into data(WBKPF).
        WBKPF-AWKEY = WBKPF-AWKEY+0(15).
        modify TBKPF from WBKPF index SY-TABIX transporting AWKEY.
      endloop.
      "
      if TBKPF[] is not initial.
        data: IT_BKPF_     type table of TY_BKPF,
              WA_BKPF_     type TY_BKPF,
              IT_ZFIT0133_ type table of ZFIT0133,
              WA_ZFIT0133_ type ZFIT0133,
              W_ANSWER(1),
              W_TEXTO(50),
              V_LINES      type I,
              V_POS1       type I,
              V_POS2       type I,
              C_LINES(10).
        select  *
           from ZFIT0133
           into table @data(T133)
          for all entries in @TBKPF
           where CHAVE_REFERENCIA eq @TBKPF-AWKEY.
        sort T133 by CHAVE_REFERENCIA.
        loop at TBKPF into WBKPF.
          read table T133 into data(W133_) with key CHAVE_REFERENCIA =  WBKPF-AWKEY binary search.
          if SY-SUBRC ne 0.
            move-corresponding  WBKPF to WA_BKPF_.
            append WA_BKPF_ to IT_BKPF_.
          endif.
        endloop.
        V_LINES = LINES( IT_BKPF_ ).
        C_LINES = V_LINES.
        concatenate 'Confirma a inclusão de'  C_LINES 'documentos' into W_TEXTO separated by SPACE.
        call function 'POPUP_TO_CONFIRM'
          exporting
            TEXT_QUESTION         = W_TEXTO
            TEXT_BUTTON_1         = 'Sim'
            ICON_BUTTON_1         = 'ICON_OKAY '
            TEXT_BUTTON_2         = 'Não'
            ICON_BUTTON_2         = 'ICON_CANCEL'
            DEFAULT_BUTTON        = '1'
            DISPLAY_CANCEL_BUTTON = ' '
            START_COLUMN          = 25
            START_ROW             = 6
          importing
            ANSWER                = W_ANSWER
          exceptions
            TEXT_NOT_FOUND        = 1
            others                = 2.

        if W_ANSWER = '1'.
* ---> S4 Migration - 15/06/2023 - MA
*          SELECT *
*            FROM BSEG
*            INTO TABLE @DATA(TBSEG)
*            FOR ALL ENTRIES IN @IT_BKPF_
*            WHERE BUKRS    EQ @IT_BKPF_-BUKRS
*            AND   BELNR    EQ @IT_BKPF_-BELNR
*            AND   GJAHR    EQ @IT_BKPF_-GJAHR.

          data LT_FIELDS type FAGL_T_FIELD.
          data: LT_BSEG type table of BSEG,
                TBSEG   type table of BSEG.

          call function 'FAGL_GET_BSEG_FOR_ALL_ENTRIES'
            exporting
              IT_FOR_ALL_ENTRIES = IT_BKPF_
              I_WHERE_CLAUSE     = |BUKRS = IT_FOR_ALL_ENTRIES-BUKRS AND BELNR = IT_FOR_ALL_ENTRIES-BELNR AND GJAHR = IT_FOR_ALL_ENTRIES-GJAHR AND |
*             IT_FIELDLIST       = LT_FIELDS
            importing
              ET_BSEG            = LT_BSEG
            exceptions
              NOT_FOUND          = 1.

          if SY-SUBRC = 0 and LINES( LT_BSEG ) > 0.
            MOVE-CORRESPONDING LT_BSEG to TBSEG.
            SY-DBCNT = LINES( LT_BSEG ).
          else.
            SY-SUBRC = 4.
            SY-DBCNT = 0.
          endif.
* <--- S4 Migration - 15/06/2023 - MA

          sort TBKPF by BUKRS BELNR GJAHR.
          loop at TBSEG into data(WBSEG).
            read table TBKPF into WBKPF with key BUKRS = WBSEG-BUKRS
                                                 BELNR = WBSEG-BELNR
                                                 GJAHR = WBSEG-GJAHR binary search.
            clear: V_POS1, V_POS2.
            search WBSEG-SGTXT for 'OPR#'.
            if SY-FDPOS gt 0.
              V_POS1 = SY-FDPOS + 4.
            else.
              search WBSEG-SGTXT for '#'.
              V_POS1 = SY-FDPOS + 1.
            endif.
            if V_POS1 = 0.
              write: / 'Erro '.
              write: WBSEG-BELNR.
              continue.
            endif.

            V_POS2 = 50 - V_POS1.
            if V_POS2 <= 0.
              write: / 'Erro '.
              write: WBSEG-BELNR.
              continue.
            endif.

            VSGTXT = WBSEG-SGTXT+V_POS1(V_POS2).
            search VSGTXT for ':'.
            V_POS2 = SY-FDPOS.
            if V_POS2 = 0 or V_POS2 gt 6.
              V_POS2 = 6.
            endif.

            VSGTXT_1 = VSGTXT+0(V_POS2).
            clear VSGTXT_2.
            do 6 times.
              subtract 1 from SY-INDEX.
              if '0123456789' cs VSGTXT_1+SY-INDEX(1).
                concatenate VSGTXT_2 VSGTXT_1+SY-INDEX(1) into VSGTXT_2.
              endif.
            enddo.
            "
            WA_ZFIT0133_-CHAVE_REFERENCIA   = WBKPF-AWKEY+0(15).
            WA_ZFIT0133_-MVT_CONTADOR       = WBSEG-XREF1 + WBSEG-BUZEI.
            WA_ZFIT0133_-CONTRATO_NUMERO    = VSGTXT_2.
            WA_ZFIT0133_-DATA_ORIGEM        = WA_BKPF_-BUDAT.
            WA_ZFIT0133_-ID_SISTEMA         = 3.
            WA_ZFIT0133_-OPERACAO_ID        = WBKPF-AWKEY+0(11).
            WA_ZFIT0133_-OPERACAO_DESCRICAO = WBSEG-SGTXT.
            WA_ZFIT0133_-DATA_BAIXA         = WBKPF-BUDAT.
            WA_ZFIT0133_-TIPO               = WBSEG-XREF3.

            concatenate 'X' WBSEG-SGTXT into WBSEG-SGTXT.
            clear V_LINES.
            search WBSEG-SGTXT for 'Transferencia Longo-Curto'.
            if SY-FDPOS eq 1.
              V_LINES = 1.
            endif.
            "
            search WBSEG-SGTXT for 'Liquidacao'.
            if SY-FDPOS eq 1.
              V_LINES = 2.
            endif.

            search WBSEG-SGTXT for 'Captacao'.
            if SY-FDPOS eq 1.
              V_LINES = 3.
            endif.

            search WBSEG-SGTXT for 'Amortização'.
            if SY-FDPOS eq 1.
              V_LINES = 4.
            endif.

            search WBSEG-SGTXT for 'Resultado'.
            if SY-FDPOS eq 1.
              V_LINES = 5.
            endif.

            search WBSEG-SGTXT for 'Apropriacao'.
            if SY-FDPOS eq 1.
              V_LINES = 6.
            endif.

            search WBSEG-SGTXT for 'Pelo BOL_'.
            if SY-FDPOS eq 1.
              V_LINES = 7.
            endif.

            search WBSEG-SGTXT for 'Pelo COB_'.
            if SY-FDPOS eq 1.
              V_LINES = 8.
            endif.

            search WBSEG-SGTXT for 'Pelo DEP_'.
            if SY-FDPOS eq 1.
              V_LINES = 9.
            endif.

            search WBSEG-SGTXT for 'Pelo TED_'.
            if SY-FDPOS eq 1.
              V_LINES = 10.
            endif.

            search WBSEG-SGTXT for 'PRE_PAGTO'.
            if SY-FDPOS eq 1.
              V_LINES = 11.
            endif.

            search WBSEG-SGTXT for 'Ajuste'.
            if SY-FDPOS eq 1.
              V_LINES = 12.
            endif.

            search WBSEG-SGTXT for 'VARIACAO'.
            if SY-FDPOS eq 1.
              V_LINES = 13.
            endif.

            if V_LINES = 0.
              continue.
            endif.

            case V_LINES.
              when 1. "'Transferencia Longo-Curto'.
                case WBSEG-BSCHL.
                  when 29.
                    "D  A
                    WA_ZFIT0133_-CREDITO_DEBITO     = 'D'.
                    WA_ZFIT0133_-APROPRIACAO_CAIXA  = 'A'.
                  when 09.
                    "D  A
                    WA_ZFIT0133_-CREDITO_DEBITO     = 'D'.
                    WA_ZFIT0133_-APROPRIACAO_CAIXA  = 'A'.
                  when 31.
                    "C  A
                    WA_ZFIT0133_-CREDITO_DEBITO     = 'C'.
                    WA_ZFIT0133_-APROPRIACAO_CAIXA  = 'A'.
                  when 19.
                    "C  A
                    WA_ZFIT0133_-CREDITO_DEBITO     = 'C'.
                    WA_ZFIT0133_-APROPRIACAO_CAIXA  = 'A'.
                endcase.
              when 2. "'Liquidacao'.
                case WBSEG-BSCHL.
                  when 21.
                    "D  C
                    WA_ZFIT0133_-CREDITO_DEBITO     = 'D'.
                    WA_ZFIT0133_-APROPRIACAO_CAIXA  = 'C'.
                  when 31.
                    "C  C
                    WA_ZFIT0133_-CREDITO_DEBITO     = 'C'.
                    WA_ZFIT0133_-APROPRIACAO_CAIXA  = 'C'.
                endcase.
              when 3. "'Captacao'.
                case WBSEG-BSCHL.
                  when 39.
                    "C  C
                    WA_ZFIT0133_-CREDITO_DEBITO     = 'C'.
                    WA_ZFIT0133_-APROPRIACAO_CAIXA  = 'C'.
                  when 31.
                    "C  C
                    WA_ZFIT0133_-CREDITO_DEBITO     = 'C'.
                    WA_ZFIT0133_-APROPRIACAO_CAIXA  = 'C'.
                endcase.
              when 4. "'Amortização'.
                case WBSEG-BSCHL.
                  when 40.
                    "D  C
                    WA_ZFIT0133_-CREDITO_DEBITO     = 'D'.
                    WA_ZFIT0133_-APROPRIACAO_CAIXA  = 'C'.
                  when 21.
                    "D  C
                    WA_ZFIT0133_-CREDITO_DEBITO     = 'D'.
                    WA_ZFIT0133_-APROPRIACAO_CAIXA  = 'C'.
                  when 50.
                    "C  C
                    WA_ZFIT0133_-CREDITO_DEBITO     = 'C'.
                    WA_ZFIT0133_-APROPRIACAO_CAIXA  = 'C'.
                  when 31.
                    "C  C
                    WA_ZFIT0133_-CREDITO_DEBITO     = 'C'.
                    WA_ZFIT0133_-APROPRIACAO_CAIXA  = 'C'.
                endcase.
              when 5. "'Resultado'.
                case WBSEG-BSCHL.
                  when 40.
                    "D  A
                    WA_ZFIT0133_-CREDITO_DEBITO     = 'D'.
                    WA_ZFIT0133_-APROPRIACAO_CAIXA  = 'A'.
                  when 50.
                    "C  A
                    WA_ZFIT0133_-CREDITO_DEBITO     = 'C'.
                    WA_ZFIT0133_-APROPRIACAO_CAIXA  = 'A'.
                endcase.
              when 6. "'Apropriacao'.
                case WBSEG-BSCHL.
                  when 09.
                    "D  A
                    WA_ZFIT0133_-CREDITO_DEBITO     = 'D'.
                    WA_ZFIT0133_-APROPRIACAO_CAIXA  = 'A'.
                  when 21.
                    "D  A
                    WA_ZFIT0133_-CREDITO_DEBITO     = 'D'.
                    WA_ZFIT0133_-APROPRIACAO_CAIXA  = 'A'.
                  when 19.
                    "C  A
                    WA_ZFIT0133_-CREDITO_DEBITO     = 'C'.
                    WA_ZFIT0133_-APROPRIACAO_CAIXA  = 'A'.
                  when 31.
                    "C  A
                    WA_ZFIT0133_-CREDITO_DEBITO     = 'C'.
                    WA_ZFIT0133_-APROPRIACAO_CAIXA  = 'A'.
                endcase.
              when 7. "'Pelo BOL'.
                case WBSEG-BSCHL.
                  when 50.
                    "C  C
                    WA_ZFIT0133_-CREDITO_DEBITO     = 'C'.
                    WA_ZFIT0133_-APROPRIACAO_CAIXA  = 'C'.
                  when 19.
                    "C  C
                    WA_ZFIT0133_-CREDITO_DEBITO     = 'C'.
                    WA_ZFIT0133_-APROPRIACAO_CAIXA  = 'C'.
                  when 40.
                    "D  C
                    WA_ZFIT0133_-CREDITO_DEBITO     = 'D'.
                    WA_ZFIT0133_-APROPRIACAO_CAIXA  = 'C'.
                  when 09.
                    "D  C
                    WA_ZFIT0133_-CREDITO_DEBITO     = 'D'.
                    WA_ZFIT0133_-APROPRIACAO_CAIXA  = 'C'.
                endcase.
              when 8. "'Pelo COB'.
                case WBSEG-BSCHL.
                  when 50.
                    "C  C
                    WA_ZFIT0133_-CREDITO_DEBITO     = 'C'.
                    WA_ZFIT0133_-APROPRIACAO_CAIXA  = 'C'.
                  when 19.
                    "C  C
                    WA_ZFIT0133_-CREDITO_DEBITO     = 'C'.
                    WA_ZFIT0133_-APROPRIACAO_CAIXA  = 'C'.
                  when 09.
                    "D C
                    WA_ZFIT0133_-CREDITO_DEBITO     = 'D'.
                    WA_ZFIT0133_-APROPRIACAO_CAIXA  = 'C'.
                  when 40.
                    "D  C
                    WA_ZFIT0133_-CREDITO_DEBITO     = 'D'.
                    WA_ZFIT0133_-APROPRIACAO_CAIXA  = 'C'.
                endcase.
              when 9. "'Pelo DEP_'.
                case WBSEG-BSCHL.
                  when 50.
                    "C  C
                    WA_ZFIT0133_-CREDITO_DEBITO     = 'C'.
                    WA_ZFIT0133_-APROPRIACAO_CAIXA  = 'C'.
                  when 19.
                    "C  C
                    WA_ZFIT0133_-CREDITO_DEBITO     = 'C'.
                    WA_ZFIT0133_-APROPRIACAO_CAIXA  = 'C'.
                  when 40.
                    "D  C
                    WA_ZFIT0133_-CREDITO_DEBITO     = 'D'.
                    WA_ZFIT0133_-APROPRIACAO_CAIXA  = 'C'.
                  when 09.
                    "D  C
                    WA_ZFIT0133_-CREDITO_DEBITO     = 'D'.
                    WA_ZFIT0133_-APROPRIACAO_CAIXA  = 'C'.
                endcase.
              when 10. "'Pelo TED_'.
                case WBSEG-BSCHL.
                  when 50.
                    "C  C
                    WA_ZFIT0133_-CREDITO_DEBITO     = 'C'.
                    WA_ZFIT0133_-APROPRIACAO_CAIXA  = 'C'.
                  when 19.
                    "C  C
                    WA_ZFIT0133_-CREDITO_DEBITO     = 'C'.
                    WA_ZFIT0133_-APROPRIACAO_CAIXA  = 'C'.
                  when 40.
                    "D  C
                    WA_ZFIT0133_-CREDITO_DEBITO     = 'D'.
                    WA_ZFIT0133_-APROPRIACAO_CAIXA  = 'C'.
                  when 09.
                    "D  C
                    WA_ZFIT0133_-CREDITO_DEBITO     = 'D'.
                    WA_ZFIT0133_-APROPRIACAO_CAIXA  = 'C'.
                endcase.
              when 11. "'PRE_PAGTO'.
                case WBSEG-BSCHL.
                  when 21.
                    "D  C
                    WA_ZFIT0133_-CREDITO_DEBITO     = 'D'.
                    WA_ZFIT0133_-APROPRIACAO_CAIXA  = 'C'.
                  when 19.
                    "C  C
                    WA_ZFIT0133_-CREDITO_DEBITO     = 'C'.
                    WA_ZFIT0133_-APROPRIACAO_CAIXA  = 'C'.
                endcase.
              when 12. "'Ajuste'.
                case WBSEG-BSCHL.
                  when 21.
                    "D  C
                    WA_ZFIT0133_-CREDITO_DEBITO     = 'D'.
                    WA_ZFIT0133_-APROPRIACAO_CAIXA  = 'C'.
                  when 31.
                    "C  C
                    WA_ZFIT0133_-CREDITO_DEBITO     = 'C'.
                    WA_ZFIT0133_-APROPRIACAO_CAIXA  = 'C'.
                endcase.
              when 13. "'VARIACAO'.
                case WBSEG-BSCHL.
                  when 31.
                    "C  C
                    WA_ZFIT0133_-CREDITO_DEBITO     = 'C'.
                    WA_ZFIT0133_-APROPRIACAO_CAIXA  = 'C'.
                  when 21.
                    "D  C
                    WA_ZFIT0133_-CREDITO_DEBITO     = 'D'.
                    WA_ZFIT0133_-APROPRIACAO_CAIXA  = 'C'.
                  when 40.
                    "D  C
                    WA_ZFIT0133_-CREDITO_DEBITO     = 'D'.
                    WA_ZFIT0133_-APROPRIACAO_CAIXA  = 'C'.
                  when 50.
                    "D  C
                    WA_ZFIT0133_-CREDITO_DEBITO     = 'D'.
                    WA_ZFIT0133_-APROPRIACAO_CAIXA  = 'C'.
                endcase.
            endcase.
            "
            WA_ZFIT0133_-VALOR_ORIGINAL_D   = WBSEG-DMBE2.
            WA_ZFIT0133_-VALOR_ORIGINAL_R   = WBSEG-DMBTR.
            WA_ZFIT0133_-VR_COTACAO         = WBSEG-DMBTR / WBSEG-DMBE2.
            WA_ZFIT0133_-ID_EMPRESA_SAP     = WBSEG-BUKRS.
            WA_ZFIT0133_-ID_FILIAL_SAP      = WBSEG-GSBER.
            WA_ZFIT0133_-PARCEIRO_ID        = WBSEG-LIFNR.
            WA_ZFIT0133_-CONTA_CONTABIL     = WBSEG-HKONT.
            WA_ZFIT0133_-PAR_CONTABIL       = WBSEG-XREF1.
            append WA_ZFIT0133_ to IT_ZFIT0133_.
          endloop.
          "
          modify ZFIT0133 from table IT_ZFIT0133_ .
          commit work.
          message 'Atualizado com sucesso' type 'I'.
          exit.
        endif.
      else.
        message 'Sem dados para processamento' type 'I'.
        exit.
      endif.

    endif.
  elseif WG_ACAO = C_DISPLA.
    if D_BUTT1 = 'BTN1'.
      select *
        from ZFIT0130
        into corresponding fields of table IT_SAIDA_0300
        where BUKRS = P_BUKRS.
    elseif D_BUTT1 = 'BTN2'.
      select *
        from ZFIT0131
           into corresponding fields of table IT_SAIDA_0200.
    else..
      select *
        from ZFIT0135
           into corresponding fields of table IT_SAIDA_0400.
    endif.
  endif.

  refresh: STYLE.
  clear: WA_STYLE.

  WA_STYLE-FIELDNAME = 'HKONT'.
  WA_STYLE-STYLE     = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED +  ALV_STYLE_FONT_BOLD.
  insert  WA_STYLE into table STYLE .

*  WA_STYLE-FIELDNAME = 'ID_SISTEMA'.
*  WA_STYLE-STYLE     = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED +  ALV_STYLE_FONT_BOLD.
*  INSERT  WA_STYLE INTO TABLE STYLE .

  loop at IT_SAIDA_0200 into WA_SAIDA_0200.
    TABIX  = SY-TABIX.
    WA_SAIDA_0200-LINHA = TABIX.
    select single *
      from SKAT
      into WL_SKAT
      where SPRAS = SY-LANGU
      and   SAKNR = WA_SAIDA_0200-HKONT.
    if SY-SUBRC = 0..
      WA_SAIDA_0200-TXT50 = WL_SKAT-TXT50.
    endif.
    WA_SAIDA_0200-STYLE[] = STYLE[].
    modify IT_SAIDA_0200 from WA_SAIDA_0200 index TABIX transporting LINHA TXT50 STYLE.
  endloop.

  refresh: STYLE.
  clear: WA_STYLE.

  WA_STYLE-FIELDNAME = 'BUKRS'.
  WA_STYLE-STYLE     = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED +  ALV_STYLE_FONT_BOLD.
  insert  WA_STYLE into table STYLE .

  WA_STYLE-FIELDNAME = 'GJAHR'.
  WA_STYLE-STYLE     = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED +  ALV_STYLE_FONT_BOLD.
  insert  WA_STYLE into table STYLE .

  WA_STYLE-FIELDNAME = 'DT_REGIME'.
  WA_STYLE-STYLE     = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED +  ALV_STYLE_FONT_BOLD.
  insert  WA_STYLE into table STYLE .

  WA_STYLE-FIELDNAME = 'REGIME'.
  WA_STYLE-STYLE     = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED +  ALV_STYLE_FONT_BOLD.
  insert  WA_STYLE into table STYLE .
  "
  loop at IT_SAIDA_0300 into WA_SAIDA_0300.
    TABIX  = SY-TABIX.
    WA_SAIDA_0300-LINHA = TABIX.
    if WA_SAIDA_0300-STATUS = 'X'.
      WA_SAIDA_0300-STATUS = ICON_INCOMPLETE.
    else.
      WA_SAIDA_0300-STATUS = ICON_CHECKED.
    endif.

    WA_SAIDA_0300-STYLE[] = STYLE[].
    modify IT_SAIDA_0300 from WA_SAIDA_0300 index TABIX transporting LINHA STATUS STYLE.
  endloop.
  "
  refresh: STYLE.
  clear: WA_STYLE.
  loop at IT_SAIDA_0400 into WA_SAIDA_0400.
    TABIX  = SY-TABIX.
    WA_SAIDA_0400-LINHA = TABIX.
    if WA_SAIDA_0400-STATUS = ICON_MESSAGE_WARNING_SMALL.
      modify IT_SAIDA_0400 from WA_SAIDA_0400 index TABIX transporting LINHA.
      continue.
    endif.

    if WA_SAIDA_0400-BUKRS is not initial and
       WA_SAIDA_0400-BELNR is not initial and
       WA_SAIDA_0400-GJAHR is not initial.
      WA_SAIDA_0400-STATUS = ICON_CHECKED.
    else.
      WA_SAIDA_0400-STATUS = ICON_INCOMPLETE.
    endif.

*    WA_SAIDA_0400-STYLE[] = STYLE[].
    modify IT_SAIDA_0400 from WA_SAIDA_0400 index TABIX transporting LINHA STATUS.
  endloop.
endform.
*&---------------------------------------------------------------------*
*&      Form  F_GRAVA_DADOS2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form F_GRAVA_DADOS2 .
  data: WL_ZFIT0130 type ZFIT0130,
        TL_ZFIT0130 type table of ZFIT0130,
        WL_ZFIT0131 type ZFIT0131,
        TL_ZFIT0131 type table of ZFIT0131,
        WL_ZFIT0135 type ZFIT0135,
        TL_ZFIT0135 type table of ZFIT0135.

  refresh: TL_ZFIT0130, TL_ZFIT0131, TL_ZFIT0135.

  if D_BUTT1 = 'BTN1'.
    loop at IT_SAIDA_0300 into WA_SAIDA_0300.
      WL_ZFIT0130-BUKRS         = WA_SAIDA_0300-BUKRS.
      WL_ZFIT0130-GJAHR         = WA_SAIDA_0300-GJAHR.
      WL_ZFIT0130-DT_REGIME     = WA_SAIDA_0300-DT_REGIME.
      WL_ZFIT0130-REGIME        = WA_SAIDA_0300-REGIME.
      WL_ZFIT0130-TX_FIXADA     = WA_SAIDA_0300-TX_FIXADA.
      WL_ZFIT0130-DT_TX_FIXADA  = WA_SAIDA_0300-DT_TX_FIXADA.
      WL_ZFIT0130-OBSERV        = WA_SAIDA_0300-OBSERV.
      if WA_SAIDA_0300-STATUS = ICON_INCOMPLETE.
        WL_ZFIT0130-STATUS = 'X'.
      else.
        WL_ZFIT0130-STATUS = ''.
      endif.
      if WA_SAIDA_0300-USUARIO is initial.
        WL_ZFIT0130-USUARIO       = SY-UNAME.
        WL_ZFIT0130-DATA_REGISTRO = SY-DATUM.
        WL_ZFIT0130-HORA_REGISTRO = SY-UZEIT.
      else.
        WL_ZFIT0130-USUARIO       = WA_SAIDA_0300-USUARIO.
        WL_ZFIT0130-DATA_REGISTRO = WA_SAIDA_0300-DATA_REGISTRO.
        WL_ZFIT0130-HORA_REGISTRO = WA_SAIDA_0300-HORA_REGISTRO.
      endif.

      append WL_ZFIT0130 to TL_ZFIT0130.
    endloop.
    modify ZFIT0130 from table TL_ZFIT0130.
  elseif D_BUTT1 = 'BTN2'.
    loop at IT_SAIDA_0200 into WA_SAIDA_0200.
      WL_ZFIT0131-HKONT         = WA_SAIDA_0200-HKONT.
      WL_ZFIT0131-ID_SISTEMA    = WA_SAIDA_0200-ID_SISTEMA.

      if WA_SAIDA_0200-USUARIO is initial.
        WL_ZFIT0131-USUARIO       = SY-UNAME.
        WL_ZFIT0131-DATA_REGISTRO = SY-DATUM.
        WL_ZFIT0131-HORA_REGISTRO = SY-UZEIT.
      else.
        WL_ZFIT0131-USUARIO       = WA_SAIDA_0200-USUARIO.
        WL_ZFIT0131-DATA_REGISTRO = WA_SAIDA_0200-DATA_REGISTRO.
        WL_ZFIT0131-HORA_REGISTRO = WA_SAIDA_0200-HORA_REGISTRO.
      endif.

      append WL_ZFIT0131 to TL_ZFIT0131.
    endloop.
    modify ZFIT0131 from table TL_ZFIT0131.
    "
  else.
    loop at IT_SAIDA_0400 into WA_SAIDA_0400.
      if WA_SAIDA_0400-BELNR is initial.
        continue.
      endif.
      move-corresponding WA_SAIDA_0400 to WL_ZFIT0135.

      if WA_SAIDA_0400-USUARIO is initial.
        WL_ZFIT0135-USUARIO       = SY-UNAME.
        WL_ZFIT0135-DATA_REGISTRO = SY-DATUM.
        WL_ZFIT0135-HORA_REGISTRO = SY-UZEIT.
      else.
        WL_ZFIT0135-USUARIO       = WA_SAIDA_0200-USUARIO.
        WL_ZFIT0135-DATA_REGISTRO = WA_SAIDA_0200-DATA_REGISTRO.
        WL_ZFIT0135-HORA_REGISTRO = WA_SAIDA_0200-HORA_REGISTRO.
      endif.

      append WL_ZFIT0135 to TL_ZFIT0135.
    endloop.
    " Para atualizar os antigos que eram apenas "E" (retirar apos ajuste na proxima request)
    update  ZFIT0135 set ENT_SAI = 'E'
    where ENT_SAI = ' '.
    commit work.

    modify ZFIT0135 from table TL_ZFIT0135.
  endif.
  commit work.

  refresh TG_MSG_RET.
  call function 'Z_DOC_CHECK_NEW'
    exporting
      I_SCREEN      = '200'
      I_SHOW        = C_X
      I_REPID       = SY-REPID
      I_PRESSED_TAB = ''
      I_SET_FIELD   = 'X_FIELD'
    importing
      E_MESSAGEM    = WG_MENSAGEM
    tables
      IT_MSGS       = TG_MSG_RET.
  message S836(SD) with text-M01  text-M02.
endform.
*&---------------------------------------------------------------------*
*&      Form  F_VERIFICA_ERROS2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form F_VERIFICA_ERROS2 .
  data: WL_LINHA(6),
        VVALOR_VALIDOI type ZFIT0134-VALOR_VALIDO,
        VVALOR_VALIDOF type ZFIT0134-VALOR_VALIDO.

  clear:    TG_MSG_RET.
  refresh:  TG_MSG_RET, IT_SAIDA_0200D, IT_SAIDA_0300D.

  if D_BUTT1 = 'BTN1'.
    loop at IT_SAIDA_0300 into WA_SAIDA_0300.
      if WA_SAIDA_0300-BUKRS     ne P_BUKRS or
         WA_SAIDA_0300-GJAHR     is initial or
         WA_SAIDA_0300-DT_REGIME is initial or
         WA_SAIDA_0300-REGIME    is initial or
         WA_SAIDA_0300-TX_FIXADA is initial or
         WA_SAIDA_0300-DT_TX_FIXADA    is initial.
        WL_LINHA = SY-TABIX.
        if WA_SAIDA_0300-BUKRS     ne P_BUKRS.
          concatenate  'Linha '  WL_LINHA 'Empresa deve ser a mesma de "critérios de seleção"' into  TG_MSG_RET-MSG separated by SPACE.
        else.
          concatenate  'Linha '  WL_LINHA 'incompleta' into  TG_MSG_RET-MSG separated by SPACE.
        endif.
        append TG_MSG_RET.
        clear: TG_MSG_RET.
      endif.
      move-corresponding WA_SAIDA_0300 to WA_SAIDA_0300D.
      WA_SAIDA_0300D-LINHA = 1.
      collect WA_SAIDA_0300D into IT_SAIDA_0300D.
    endloop.
    delete IT_SAIDA_0300D where LINHA eq 1.
    loop at IT_SAIDA_0300D into WA_SAIDA_0300D.
      loop at  IT_SAIDA_0300 into WA_SAIDA_0300 where BUKRS     = WA_SAIDA_0300D-BUKRS
                                                and   GJAHR     = WA_SAIDA_0300D-GJAHR
                                                and   DT_REGIME = WA_SAIDA_0300D-DT_REGIME
                                                and   REGIME    = WA_SAIDA_0300D-REGIME.
        if  WA_SAIDA_0300-STYLE[] is initial.
          WL_LINHA = WA_SAIDA_0300-LINHA..
          concatenate  'Linha '  WL_LINHA 'duplicada' into  TG_MSG_RET-MSG separated by SPACE.
          append TG_MSG_RET.
          clear: TG_MSG_RET.
        endif.
      endloop.

    endloop.
  elseif D_BUTT1 = 'BTN2'.
    loop at IT_SAIDA_0200 into WA_SAIDA_0200.
      if WA_SAIDA_0200-TXT50 is initial.
        WL_LINHA = SY-TABIX.
        concatenate  WA_SAIDA_0200-HKONT 'não existe '  WL_LINHA into  TG_MSG_RET-MSG separated by SPACE.
        append TG_MSG_RET.
        clear: TG_MSG_RET.
      endif.
      move-corresponding WA_SAIDA_0200 to WA_SAIDA_0200D.
      WA_SAIDA_0200D-LINHA = 1.
      collect WA_SAIDA_0200D into IT_SAIDA_0200D.
    endloop.
    delete IT_SAIDA_0200D where LINHA eq 1.
    loop at IT_SAIDA_0200D into WA_SAIDA_0200D.
      loop at  IT_SAIDA_0200 into WA_SAIDA_0200 where HKONT     = WA_SAIDA_0200D-HKONT.
        if  WA_SAIDA_0200-STYLE[] is initial.
          WL_LINHA = WA_SAIDA_0200-LINHA..
          concatenate  'Linha '  WL_LINHA 'duplicada' into  TG_MSG_RET-MSG separated by SPACE.
          append TG_MSG_RET.
          clear: TG_MSG_RET.
        endif.
      endloop.

    endloop.
  else.
    loop at IT_SAIDA_0400 into WA_SAIDA_0400.
      if WA_SAIDA_0400-BELNR is not initial.
        VVALOR_VALIDOI = WA_SAIDA_0400-VALOR_VALIDO - 1.
        VVALOR_VALIDOF = WA_SAIDA_0400-VALOR_VALIDO + 1.
        select single *
          into @data(WBSAK)
          from BSAK
          where BUKRS = @WA_SAIDA_0400-BUKRS
          and   BELNR = @WA_SAIDA_0400-BELNR
          and   GJAHR = @WA_SAIDA_0400-GJAHR
          and   WRBTR between @VVALOR_VALIDOI and @VVALOR_VALIDOF.
        if SY-SUBRC ne 0.
          select single *
            into @data(WBSIK)
            from BSIK
            where BUKRS = @WA_SAIDA_0400-BUKRS
            and   BELNR = @WA_SAIDA_0400-BELNR
            and   GJAHR = @WA_SAIDA_0400-GJAHR
          and   WRBTR between @VVALOR_VALIDOI and @VVALOR_VALIDOF.
          if SY-SUBRC ne 0.
            WL_LINHA = WA_SAIDA_0400-LINHA..
            concatenate  'Linha '  WL_LINHA 'documento não existe' into  TG_MSG_RET-MSG separated by SPACE.
            append TG_MSG_RET.
            clear: TG_MSG_RET.
          endif.
        endif.
      endif.
    endloop.
  endif.

endform.

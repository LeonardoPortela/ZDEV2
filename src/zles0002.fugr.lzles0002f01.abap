*----------------------------------------------------------------------*
***INCLUDE LZLES0002F01 .
*----------------------------------------------------------------------*

***********************************************************************
* Tabelas transparentes
***********************************************************************
tables: ZLEST0008,
        ZLEST0022.

*&---------------------------------------------------------------------*
*&      Form  YF_INICIALIZACAO
*&---------------------------------------------------------------------*
form YF_INICIALIZACAO  tables
                         PT_LOTES    type ZLES_COCKPIT_LOTE_T
                         PT_LANCTOS  type ZLES_COCKPIT_LANCTO_T
                         PT_DELTAS   type ZLES_COCKPIT_DELTA_T
                         PT_CONFER   type ZLES_COCKPIT_CONFER_T
                         PT_ACRDECR  type ZLES_COCKPIT_ACRESCDECRES_T
                         PT_MSG      structure BAPIRET2
                       using
                         PR_TRASNPORTADOR  type LXHME_RANGE_C10_T
                         PR_POSTO          type LXHME_RANGE_C10_T
                         PR_LOTE           type LXHME_RANGE_C10_T
                         PR_CONHECIMENTO   type LXHME_RANGE_C10_T
                         PR_CARTA_FRETE    type LXHME_RANGE_C10_T
                         PR_PERIODO        type LXHME_RANGE_DATE_T
                         PR_FECHAMENTO     type LXHME_RANGE_DATE_T
                         PR_VENCIMENTO     type LXHME_RANGE_DATE_T
                         PR_STATUS         type LXHME_RANGE_C1_T
                     changing PE_MSGERR        type BAPI_MSG.


* Inicialização
  clear: PE_MSGERR.

  refresh: PT_LOTES,
           PT_LANCTOS,
           PT_DELTAS,
           PT_CONFER,
           PT_ACRDECR.

* Referência de valores por ponteiros
  assign: PT_LOTES[]        to  <TI_LOTES>,
          PT_LANCTOS[]      to  <TI_LANCTOS>,
          PT_DELTAS[]       to  <TI_DELTAS>,
          PT_CONFER[]       to  <TI_CONFER>,
          PT_ACRDECR[]      to  <TI_ACRDCR>,
          PR_CONHECIMENTO   to  <RC_CONHECIMENTO>,
          PR_CARTA_FRETE    to  <RC_CARTA_FRETE>,
          PR_PERIODO        to  <RC_PERIODO>,
          PR_FECHAMENTO     to  <RC_FECHAMENTO>,
          PR_VENCIMENTO     to  <RC_VENCIMENTO>,
          PR_STATUS         to  <RC_STATUS_LOTE>,
          PE_MSGERR         to  <VC_MSG_ERRO>,
          PT_MSG[]          to  <TI_MSG>.

* Cópia de valores por ponteiros para alterações parâmetros de entrada
* Aplicar rotina de conversão
  create data: VG_DATAREF_RTRANSP type LXHME_RANGE_C10_T,
               VG_DATAREF_RPOSTO  type LXHME_RANGE_C10_T,
               VG_DATAREF_LOTE    type LXHME_RANGE_C10_T.

  assign: VG_DATAREF_RTRANSP->* to <RC_TRANSPORTADOR>,
          VG_DATAREF_RPOSTO->*  to <RC_POSTO>,
          VG_DATAREF_LOTE->*    to <RC_LOTE>.

  <RC_TRANSPORTADOR>[] = PR_TRASNPORTADOR[].
  <RC_POSTO>[]         = PR_POSTO[].
  <RC_LOTE>[]          = PR_LOTE[].

* Converte ranges para documento de acesso
  perform YF_RANGE_CONVERSION_ALPHA: using <RC_TRANSPORTADOR>,
                                     using <RC_POSTO>,
                                     using <RC_LOTE>.
* Limpa base de seleção
  refresh: TI_ZLEST0008,
           TI_ZLEST0013,
           TI_ZLEST0015,
           TI_ZLEST0016,
           TI_ZLEST0020.

endform.                    " YF_INICIALIZACAO

*&---------------------------------------------------------------------*
*&      Form  YF_RANGE_CONVERSION_ALPHA
*&---------------------------------------------------------------------*
form YF_RANGE_CONVERSION_ALPHA  using PT_RANGE type any table.

  field-symbols: <RANGE_WORK_AREA> type LXHME_RANGE_C10.

* Conversão do código da transportadora
  loop at PT_RANGE assigning <RANGE_WORK_AREA>.
    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        INPUT  = <RANGE_WORK_AREA>-LOW
      importing
        OUTPUT = <RANGE_WORK_AREA>-LOW.

    check not <RANGE_WORK_AREA>-LOW is initial.

    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        INPUT  = <RANGE_WORK_AREA>-HIGH
      importing
        OUTPUT = <RANGE_WORK_AREA>-HIGH.
  endloop.

endform.                    " YF_RANGE_CONVERSION_ALPHA

*&---------------------------------------------------------------------*
*&      Form  YF_BUSCA_HDR_CONHECIMENTOS
*&---------------------------------------------------------------------*
form YF_BUSCA_HDR_CONHECIMENTOS .

  perform YF_SEPARA_STATUS_CONHEC.
  perform YF_BUSCA_CONHECIMENTOS.

endform.                    " YF_BUSCA_HDR_CONHECIMENTOS

*&---------------------------------------------------------------------*
*&      Form  YF_SEPARA_STATUS_CONHEC
*&---------------------------------------------------------------------*
form YF_SEPARA_STATUS_CONHEC .

  data: LW_RANGE    type LXHME_RANGE_C1.

  refresh: RC_STAT_013,
           RC_STAT_015.

* Regras para seleção da base de dados:
* Para status = 'I' -> ZLEST0013 (rc_stat_013)
* Caso Contrário    -> ZLEST0015 (rc_stat_015)
  loop at <RC_STATUS_LOTE> into LW_RANGE.

*   Para simplificar separa somente condição SIGN=I (Include)
*   com opção OPTION=EQ ou CP (Equal or Contain Pattern - Wildcard)
*   Demais combinações acessa as duas bases de dados
    if LW_RANGE-SIGN = CC_I
      and ( LW_RANGE-OPTION = CC_EQ or LW_RANGE-OPTION = CC_CP ).
      if LW_RANGE-LOW = CC_I. "Status importação do posto
        append LW_RANGE to RC_STAT_013.
      elseif LW_RANGE-LOW co 'AC'.
        append LW_RANGE to RC_STAT_015.
      else.
        append LW_RANGE to RC_STAT_013.
        append LW_RANGE to RC_STAT_015.
      endif.
    else.
      append LW_RANGE to RC_STAT_013.
      append LW_RANGE to RC_STAT_015.
    endif.

  endloop.

endform.                    " YF_SEPARA_STATUS_CONHEC

*&---------------------------------------------------------------------*
*&      Form  YF_BUSCA_CONHECIMENTOS
*&---------------------------------------------------------------------*
form YF_BUSCA_CONHECIMENTOS .

  data: VL_BUKRS     like ZLEST0015-BUKRS,
        VL_DOCSAP    like ZLEST0015-DOCSAP,
        VL_GJAHR     like ZLEST0015-GJAHR,
        VL_ERRO_ADTO type C1.
  data: LI_TABIX            type I.
  field-symbols: <DATA> like line of TI_ZLEST0015.

  clear: <VC_MSG_ERRO>.

* Dados importados do Posto de Gasolina
  if ( not RC_STAT_013[] is initial ) or
     ( RC_STAT_013[] is initial and RC_STAT_015[] is initial ).

    if ( 'I' in <RC_STATUS_LOTE> ) or ( 'A' in <RC_STATUS_LOTE> ).
      select CNPJ_TRP CNPJ_POSTO LOTE      CTAFRETE
             CONHEC   CHVID      CODTRP    CODPOSTO
             DATALOTE VLRLOTE    VLRCONHEC QTDE
             DTACHEG  STATUS
        into table TI_ZLEST0013
        from ZLEST0013
       where CODTRP   in <RC_TRANSPORTADOR>
         and CODPOSTO in <RC_POSTO>
         and LOTE     in <RC_LOTE>
         and CONHEC   in <RC_CONHECIMENTO>
         and CTAFRETE in <RC_CARTA_FRETE>
*        AND datalote IN <rc_periodo>   "Data informada pelo posto
         and DATA     in <RC_PERIODO>   "Data do processamento
         and STATUS   in RC_STAT_013.
    else.

      select CNPJ_TRP CNPJ_POSTO LOTE      CTAFRETE
             CONHEC   CHVID      CODTRP    CODPOSTO
             DATALOTE VLRLOTE    VLRCONHEC QTDE
             DTACHEG  STATUS
        into table TI_ZLEST0013
        from ZLEST0013
       where CODTRP   in <RC_TRANSPORTADOR>
         and CODPOSTO in <RC_POSTO>
         and LOTE     in <RC_LOTE>
         and CONHEC   in <RC_CONHECIMENTO>
         and CTAFRETE in <RC_CARTA_FRETE>
*        AND datalote IN <rc_periodo>   "Data informada pelo posto
         and STATUS   in RC_STAT_013.

    endif.

  endif.

* Dados A confirmar ou confirmados para Postos - Lotes
  if not RC_STAT_015[] is initial or
    ( RC_STAT_015[] is initial and RC_STAT_013[] is initial ).


    if ( 'C' in <RC_STATUS_LOTE> ).

      select *
        into table TI_ZLEST0015
        from ZLEST0015
       where TRANSPORTADOR  in <RC_TRANSPORTADOR>
         and POSTO          in <RC_POSTO>
         and LOTE           in <RC_LOTE>
         "AND data           IN <rc_periodo>
         and DATAFECHAMENTO in <RC_FECHAMENTO>
         and VENCIMENTO     in <RC_VENCIMENTO>
         and STATUS         in RC_STAT_015.
    elseif ( 'A' in <RC_STATUS_LOTE> ).
      select *
        into table TI_ZLEST0015
        from ZLEST0015
       where TRANSPORTADOR  in <RC_TRANSPORTADOR>
         and POSTO          in <RC_POSTO>
         and LOTE           in <RC_LOTE>
         and DATA           in <RC_PERIODO>
         and VENCIMENTO     in <RC_VENCIMENTO>
         and STATUS         in RC_STAT_015.
    else.
      select *
        into table TI_ZLEST0015
        from ZLEST0015
       where TRANSPORTADOR  in <RC_TRANSPORTADOR>
         and POSTO          in <RC_POSTO>
         and LOTE           in <RC_LOTE>
         and DATA           in <RC_PERIODO>
         and STATUS         in RC_STAT_015.
    endif.


    if SY-SUBRC is initial.
      sort TI_ZLEST0015  by TRANSPORTADOR POSTO LOTE STATUS DATA.
    endif.

    loop at TI_ZLEST0015 assigning <DATA>.
      LI_TABIX = SY-TABIX.
*     Verificando se o documento de adto foi realmente criado ou nao.. e recuperando o numero(belnr) para a tabela de lote
      if ( <DATA>-DOCSAP is initial and <DATA>-OBJ_KEY is not initial ).

        clear: VL_DOCSAP, VL_BUKRS, VL_GJAHR, VL_ERRO_ADTO.

*     Recuperando o documento contabil gerado..
*        SELECT SINGLE belnr bukrs gjahr INTO (vl_docsap, vl_bukrs, vl_gjahr)
*          FROM zib_contabil_chv
*         WHERE obj_key = <data>-obj_key.

        select single BELNR BUKRS GJAHR into (VL_DOCSAP, VL_BUKRS, VL_GJAHR)
          from BKPF
         where AWKEY = <DATA>-OBJ_KEY.

*     Analisa LOG de Erro na geração do documento contábil
        if VL_DOCSAP is initial.
*       Gera Log cockpit e libera o lote novamente se houver erro na criacao do adto..
          VL_ERRO_ADTO = SPACE.
          perform LANCTO_GERA_LOGLACTO_COCKPIT changing  VL_ERRO_ADTO.
          if VL_ERRO_ADTO = 'X'.
            delete TI_ZLEST0015 index LI_TABIX.
          endif.
        else.
*   Atualiza status do documento
          <DATA>-DOCSAP = VL_DOCSAP.
          <DATA>-GJAHR  = VL_GJAHR.

          update ZLEST0015
          set DOCSAP = VL_DOCSAP
              GJAHR  = VL_GJAHR
              BUKRS  = VL_BUKRS
          where TRANSPORTADOR = <DATA>-TRANSPORTADOR
            and POSTO         = <DATA>-POSTO
            and LOTE          = <DATA>-LOTE.

          commit work and wait.
        endif.
      endif.

    endloop.

  endif.

* Verifica seleção
  if TI_ZLEST0013[] is initial and TI_ZLEST0015[] is initial.
    <VC_MSG_ERRO> = text-M01.
  endif.

endform.                    " YF_BUSCA_CONHECIMENTOS

*&---------------------------------------------------------------------*
*&      Form  YF_BUSCA_DTL_CONHECIMENTOS
*&---------------------------------------------------------------------*
form YF_BUSCA_DTL_CONHECIMENTOS .

  perform YF_BUSCA_DTL_LOTE.
  perform YF_BUSCA_DADOSADIC_CONHEC.

  perform YF_BUSCA_DELTAS_ZLEST0016.

  perform YF_BUSCA_DTL_TRANPORTE.

  perform YF_BUSCA_DTL_REMESSA.
  perform YF_BUSCA_DOCTOS_NF.

endform.                    " YF_BUSCA_DTL_CONHECIMENTOS

*&---------------------------------------------------------------------*
*&      Form  YF_BUSCA_DTL_LOTE
*&---------------------------------------------------------------------*
form YF_BUSCA_DTL_LOTE .

  data: LI_TABIX            type I.

  check: not TI_ZLEST0015[] is initial.

* Determinação de detalhes do lote A confirmar ou confirmados
  select *
    into table TI_ZLEST0016
    from ZLEST0016
     for all entries in TI_ZLEST0015
   where TRANSPORTADOR = TI_ZLEST0015-TRANSPORTADOR
     and POSTO         = TI_ZLEST0015-POSTO
     and LOTE          = TI_ZLEST0015-LOTE
     and CTAFRETE     in <RC_CARTA_FRETE>
     and CONHECIMENTO in <RC_CONHECIMENTO>.

  sort TI_ZLEST0016 by TRANSPORTADOR POSTO LOTE CHVID
                       CONHECIMENTO CTAFRETE.

* Compatibiliza seleção do header com detalhes
  if not <RC_CARTA_FRETE>[] is initial or
     not <RC_CONHECIMENTO>[] is initial.

    loop at TI_ZLEST0015.
      LI_TABIX = SY-TABIX.
      read table TI_ZLEST0016
           with key TRANSPORTADOR = TI_ZLEST0015-TRANSPORTADOR
                    POSTO         = TI_ZLEST0015-POSTO
                    LOTE          = TI_ZLEST0015-LOTE
      binary search.
      check SY-SUBRC <> 0.
      delete TI_ZLEST0015 index LI_TABIX.
    endloop.

  endif.

* Verifica seleção após compatibilização
  if TI_ZLEST0015[] is initial and TI_ZLEST0013[] is initial.
    <VC_MSG_ERRO> = text-M01.
  endif.

endform.                    " YF_BUSCA_DTL_LOTE

*&---------------------------------------------------------------------*
*&      Form  YF_BUSCA_DADOSADIC_CONHEC
*&---------------------------------------------------------------------*
form YF_BUSCA_DADOSADIC_CONHEC .

  data: begin of LT_LOTE        occurs 0,
          LOTE type CHAR10,
        end   of LT_LOTE,
        begin of LT_CHVID        occurs 0,
          CHVID type ZCHVID,
        end   of LT_CHVID,
        begin of LT_CONHEC_1     occurs 0,
          CODTRP type ZCODTRP,
          EXTI1  type EXTI1,
          EXTI2  type EXTI2,
        end   of LT_CONHEC_1,
        begin of LT_CONHEC_2     occurs 0,
          CODTRP   type ZCODTRP,
          CODPOSTO type ZCODPOSTO,
        end   of LT_CONHEC_2.

* Converte chave para compatibilidade de acesso
* 1) Tamanho conhec na tabela ZLEST0013 é diferente VTTPK
* 2) Tira duplicidades para acesso a base de dados
  check: not TI_ZLEST0013[]    is initial
     or  not TI_ZLEST0015[]    is initial
     or  not TI_ZLEST0016[]    is initial.

* Dados importados do Posto de Gasolina
  loop at TI_ZLEST0013.
    append TI_ZLEST0013-LOTE       to LT_LOTE.
    append TI_ZLEST0013-CHVID      to LT_CHVID.
    move: TI_ZLEST0013-CODTRP      to LT_CONHEC_1-CODTRP,
          TI_ZLEST0013-CONHEC      to LT_CONHEC_1-EXTI1,
          TI_ZLEST0013-CTAFRETE    to LT_CONHEC_1-EXTI2.
    append LT_CONHEC_1.
    move: TI_ZLEST0013-CODTRP      to LT_CONHEC_2-CODTRP,
          TI_ZLEST0013-CODPOSTO    to LT_CONHEC_2-CODPOSTO.
    append LT_CONHEC_2.
  endloop.

* Dados A confirmar ou confirmados Postos - Lotes (Detalhe)
  loop at TI_ZLEST0016.
    append TI_ZLEST0016-CHVID      to LT_CHVID.
  endloop.

* Dados A confirmar ou confirmados Postos - Lotes (Header)
  loop at TI_ZLEST0015.
    append TI_ZLEST0015-LOTE    to LT_LOTE.
    move: TI_ZLEST0015-TRANSPORTADOR   to LT_CONHEC_2-CODTRP,
          TI_ZLEST0015-POSTO           to LT_CONHEC_2-CODPOSTO.
    append LT_CONHEC_2.
  endloop.

* Classifica dados de conhecimento para acesso adicionais
  sort: LT_LOTE,
        LT_CHVID,
        LT_CONHEC_1,
        LT_CONHEC_2.

  delete adjacent duplicates from: LT_LOTE,
                                   LT_CHVID,
                                   LT_CONHEC_1,
                                   LT_CONHEC_2.

* Log de Erro para Lote de conhecimentos
  if not LT_LOTE[] is initial.
    select *
      into table TI_ZLEST0008
      from ZLEST0008
       for all entries in LT_LOTE
     where LOTE = LT_LOTE-LOTE
       and MSGTYP <> CC_S.
  endif.

* Controle chave de identificação de lote
  if not LT_CHVID[] is initial.
    select *
      into table TI_ZLEST0025
      from ZLEST0025
       for all entries in LT_CHVID
     where CHVID = LT_CHVID-CHVID.

*   Classifica Histórico
    sort: TI_ZLEST0025  by CHVID CALCOCKPIT.
  endif.

* Dados de transporte / valor de adiantamento (somente para zlest0013)
  if not LT_CONHEC_1[] is initial.
    select T1~TKNUM  T1~TDLNR T1~EXTI1 T1~EXTI2 T1~TPLST T1~ADD02
           T2~BUKRS T3~BEZEI
      into table TI_TRANSPORTE
      from VTTK as T1
       left outer join TTDS as T2
        on T2~TPLST = T1~TPLST
      left outer join TTDST as T3
        on T3~SPRAS = SY-LANGU
       and T3~TPLST = T1~TPLST
       for all entries in LT_CONHEC_1
     where T1~TDLNR = LT_CONHEC_1-CODTRP
       and T1~EXTI1 = LT_CONHEC_1-EXTI1
       and T1~EXTI2 = LT_CONHEC_1-EXTI2.

    if SY-SUBRC is initial.
      sort TI_TRANSPORTE by TKNUM TDLNR EXTI1 EXTI2.
      delete adjacent duplicates from TI_TRANSPORTE
                      comparing TKNUM TDLNR EXTI1 EXTI2.
    endif.
  endif.

* Dados da transportadora e do Posto
  if not LT_CONHEC_2[] is initial.

    select LIFNR STCD1 NAME1
      into table TI_TRANSPOSTO
      from LFA1
       for all entries in LT_CONHEC_2
     where LIFNR = LT_CONHEC_2-CODTRP
        or LIFNR = LT_CONHEC_2-CODPOSTO.

    sort TI_TRANSPOSTO by LIFNR.

  endif.

endform.                    " YF_BUSCA_DADOSADIC_CONHEC

*&---------------------------------------------------------------------*
*&      Form  YF_BUSCA_DELTAS_ZLEST0016
*&---------------------------------------------------------------------*
form YF_BUSCA_DELTAS_ZLEST0016 .

  data: begin of LT_DELTA  occurs 0,
          CODTRP   type ZCODTRP,
          CODPOSTO type ZCODPOSTO,
          LOTE     type CHAR10,
          CHVID    type ZCHVID,
          CONHEC   type ZCONHEC,
        end of LT_DELTA.

  check: not TI_ZLEST0016[] is initial
    and  not TI_ZLEST0025[] is initial.

* Busca Histórico (ChvId) que permitem valores Deltas
  loop at TI_ZLEST0016.
    read table TI_ZLEST0025 with key CHVID = TI_ZLEST0016-CHVID
                                CALCOCKPIT = CC_S
    binary search.
    move: TI_ZLEST0016-TRANSPORTADOR to LT_DELTA-CODTRP,
          TI_ZLEST0016-POSTO         to LT_DELTA-CODPOSTO,
          TI_ZLEST0016-LOTE          to LT_DELTA-LOTE,
          TI_ZLEST0016-CHVID         to LT_DELTA-CHVID,
          TI_ZLEST0016-CONHECIMENTO  to LT_DELTA-CONHEC.
    append LT_DELTA.
  endloop.

  check: not LT_DELTA[] is initial.

* Extrai valores de calculos do lote (Deltas)
  select *
    into table TI_ZLEST0020
    from ZLEST0020
     for all entries in LT_DELTA
   where TRANSPORTADOR = LT_DELTA-CODTRP
     and POSTO         = LT_DELTA-CODPOSTO
     and LOTE          = LT_DELTA-LOTE
     and CHVID         = LT_DELTA-CHVID
     and CONHECIMENTO  = LT_DELTA-CONHEC.

endform.                    " YF_BUSCA_DELTAS_ZLEST0016

*&---------------------------------------------------------------------*
*&      Form  YF_BUSCA_DTL_TRANPORTE
*&---------------------------------------------------------------------*
form YF_BUSCA_DTL_TRANPORTE .

  check: not TI_TRANSPORTE[] is initial.

* Custo de frete
  select FKNUM FKPOS KNUMV KZWI1 KZWI2 REBEL WERKS
    into table TI_VFKP
    from VFKP
     for all entries in TI_TRANSPORTE
   where REBEL = TI_TRANSPORTE-TKNUM
     and REFTY = CC_REFTY_8
     and FKPTY = CC_FKPTY_Z001.

* Condições de preço
  if not TI_VFKP[] is initial.

    perform YF_MONTA_RANGE_KSCHL.

    select from V_KONV fields KNUMV , KWERT , KBETR , KPEIN , KMEIN , KSCHL , KINAK for all entries in @TI_VFKP where KNUMV = @TI_VFKP-KNUMV and KSCHL in @RC_KSCHL and KINAK ne 'Y' into table @TI_KONV .

  endif.

* Obtem itens de transporte e dados de remessa
  select T1~TKNUM T1~VBELN T2~BTGEW  T2~GEWEI
    into table TI_REMESSA
    from VTTP as T1
    left outer join LIKP as T2
      on T2~VBELN = T1~VBELN
     for all entries in TI_TRANSPORTE
   where T1~TKNUM = TI_TRANSPORTE-TKNUM.

endform.                    " YF_BUSCA_DTL_TRANPORTE

*&---------------------------------------------------------------------*
*&      Form  YF_MONTA_RANGE_KSCHL
*&---------------------------------------------------------------------*
form YF_MONTA_RANGE_KSCHL .

  refresh: RC_KSCHL.
  clear:   RC_KSCHL.

  RC_KSCHL-SIGN = CC_I.
  RC_KSCHL-OPTION = CC_EQ.

  RC_KSCHL-LOW = 'ZFRE'.
  append RC_KSCHL.

  RC_KSCHL-LOW = 'ZSET'.
  append RC_KSCHL.

  RC_KSCHL-LOW = 'ZINS'.
  append RC_KSCHL.

  RC_KSCHL-LOW = 'ZIRF'.
  append RC_KSCHL.

  RC_KSCHL-LOW = 'ZADM'.
  append RC_KSCHL.

  RC_KSCHL-LOW = 'ZIOF'.
  append RC_KSCHL.

  RC_KSCHL-LOW = 'ZSEG'.
  append RC_KSCHL.

endform.                    " YF_MONTA_RANGE_KSCHL

*&---------------------------------------------------------------------*
*&      Form  YF_BUSCA_DTL_REMESSA
*&---------------------------------------------------------------------*
form YF_BUSCA_DTL_REMESSA .

  ranges: RG_VBTYP_N for VBFA-VBTYP_N occurs 0.
  data: LW_RANGE      type LXHME_RANGE_C1.

  check: not TI_REMESSA[] is initial.


* Obter Peso confirmado Hermasa
  select CH_REFERENCIA TP_MOVIMENTO NR_ROMANEIO
         VBELN         DT_MOVIMENTO NR_SAFRA
         BUKRS         BRANCH       PARID
         ID_CLI_DEST   TP_FRETE     MATNR
         PESO_LIQ      PESO_FISCAL  NFNUM
         SERIES        DOCDAT       NETWR
         NFE           DOC_REM      ID_INTERFACE
    into table TI_ZSDT0001
    from ZSDT0001
     for all entries in TI_REMESSA
   where TP_MOVIMENTO = CC_E
     and DOC_REM      = TI_REMESSA-VBELN.

  LW_RANGE-SIGN   = CC_I.
  LW_RANGE-OPTION = CC_EQ.
  LW_RANGE-LOW    = CC_VBTYPN_R.
  append LW_RANGE to RG_VBTYP_N.
  LW_RANGE-LOW    = CC_VBTYPN_M.
  append LW_RANGE to RG_VBTYP_N.

* Documentos de faturamentos
  select VBELV POSNV VBELN POSNN VBTYP_N MJAHR
    into table TI_VBFA
    from VBFA
     for all entries in TI_REMESSA
   where VBELV   = TI_REMESSA-VBELN
     and VBTYP_V = CC_VBTYPV_J
     and VBTYP_N in RG_VBTYP_N.

* Busca dados de tolerância para os documentos de remessa
  perform YF_BUSCA_TOLERANCIA.

endform.                    " YF_BUSCA_DTL_REMESSA

*&---------------------------------------------------------------------*
*&      Form  YF_BUSCA_TOLERANCIA
*&---------------------------------------------------------------------*
form YF_BUSCA_TOLERANCIA .

* Tolerância
  refresh TI_TOLERANCIA.

* Dados de fornecimento e condição para tolerância
  select distinct T1~VBELN T1~MATNR T2~KNUMH T3~KBETR
    into table TI_MARGTOLER
    from LIPS as T1
   inner join A912 as T2
      on T2~KAPPL = CC_KAPPL
     and T2~KSCHL = CC_KSCHL_ZMRG
     and T2~MATNR = T1~MATNR
     and T2~DATAB <= SY-DATUM
     and T2~DATBI >= SY-DATUM
   inner join KONP as T3
      on T3~KNUMH = T2~KNUMH
     for all entries in TI_REMESSA
   where T1~VBELN = TI_REMESSA-VBELN.

  check: not TI_MARGTOLER[] is initial.
  sort TI_MARGTOLER by VBELN.

* Extrai margem de tolerância para organização de transporte
  loop at TI_REMESSA.
*   Obtem valor condição tolerância do primeiro material da remessa 1:1
    read table TI_MARGTOLER with key VBELN = TI_REMESSA-VBELN
                            binary search.
    check SY-SUBRC is initial.
*   Verifica documento de transporte
    read table TI_TRANSPORTE with key TKNUM = TI_REMESSA-TKNUM
                             binary search.
    check SY-SUBRC is initial.
    TI_TOLERANCIA-TPLST = TI_TRANSPORTE-TPLST.
    TI_TOLERANCIA-BUKRS = TI_TRANSPORTE-BUKRS.
    TI_TOLERANCIA-TOLERANCIA = TI_MARGTOLER-KBETR / 10.
    append TI_TOLERANCIA.
  endloop.

  free TI_MARGTOLER.

endform.                    " YF_BUSCA_TOLERANCIA

*&---------------------------------------------------------------------*
*&      Form  YF_BUSCA_DOCTOS_NF
*&---------------------------------------------------------------------*
form YF_BUSCA_DOCTOS_NF .

  ranges: RG_REFTYP for J_1BNFLIN-REFTYP occurs 0.
  data: LW_RANGE  type LXHME_RANGE_C2,
        VL_REFKEY type J_1BNFLIN-REFKEY.

  data: LI_INDEX       type I.

  check: not TI_VBFA[] is initial.

  LW_RANGE-SIGN   = CC_I.
  LW_RANGE-OPTION = CC_EQ.
  LW_RANGE-LOW    = CC_REFTYP_MD.
  append LW_RANGE to RG_REFTYP.
  LW_RANGE-LOW    = CC_REFTYP_BI.
  append LW_RANGE to RG_REFTYP.

  refresh TI_NFBALANCA.
  clear:  TI_NFBALANCA.

* Documentos de NF de itens
  loop at TI_VBFA.
    if TI_VBFA-VBTYP_N = 'R'.
      concatenate TI_VBFA-VBELN TI_VBFA-REFKEY into VL_REFKEY.
      TI_VBFA-REFKEY = VL_REFKEY.
    else.
      TI_VBFA-REFKEY = TI_VBFA-VBELN.
    endif.
    modify TI_VBFA index SY-TABIX transporting REFKEY.
  endloop.

  select distinct REFKEY DOCNUM MEINS
    into table TI_J_1BNFLIN
    from J_1BNFLIN
     for all entries in TI_VBFA
   where REFTYP in RG_REFTYP
     and REFKEY = TI_VBFA-REFKEY
     and REFITM = TI_VBFA-POSNN.

* Documentos de NF de cabeçalho
  check: not TI_J_1BNFLIN[] is initial.

  select DOCNUM NFENUM NFNUM SERIES BUKRS  BRANCH GEWEI
    into table TI_NOTAFISCAL
    from J_1BNFDOC
     for all entries in TI_J_1BNFLIN
   where DOCNUM = TI_J_1BNFLIN-DOCNUM.

  loop at TI_NOTAFISCAL.

    LI_INDEX = SY-TABIX.

    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        INPUT  = TI_NOTAFISCAL-NFENUM
      importing
        OUTPUT = TI_NOTAFISCAL-NFENUM.

    concatenate TI_NOTAFISCAL-BUKRS  '-'
                TI_NOTAFISCAL-BRANCH '-'
                TI_NOTAFISCAL-NFENUM
                TI_NOTAFISCAL-NFNUM
           into TI_NOTAFISCAL-CHAVE.

    modify TI_NOTAFISCAL index LI_INDEX transporting CHAVE.
  endloop.

* Dados de balança
  check: not TI_NOTAFISCAL[] is initial.

  select CHAVE DTACHEGADA PESODVAGAO
    into table TI_BALANCA
    from ZLEST0019
     for all entries in TI_NOTAFISCAL
   where IDINTER = CC_IDINTER_L1
     and TP_MOVI = CC_E
     and TP_REG  = CC_TP_REG_30
     and CHAVE   = TI_NOTAFISCAL-CHAVE.

  sort TI_BALANCA by CHAVE.

  loop at TI_NOTAFISCAL.
    move-corresponding TI_NOTAFISCAL to TI_NFBALANCA.
    read table TI_BALANCA with key CHAVE = TI_NOTAFISCAL-CHAVE
         binary search.
    if SY-SUBRC is initial.
      move: TI_BALANCA-DTACHEGADA  to TI_NFBALANCA-DATA_CHEGA,
            TI_BALANCA-PESODVAGAO  to TI_NFBALANCA-PESO_CHEGA.
    endif.
    append TI_NFBALANCA.
    clear  TI_NFBALANCA.
  endloop.

  free: TI_NOTAFISCAL,
        TI_BALANCA.

endform.                    " YF_BUSCA_DOCTOS_NF

*&---------------------------------------------------------------------*
*&      Form  YF_JUNCAO_CONHECIMENTOS
*&---------------------------------------------------------------------*
form YF_JUNCAO_CONHECIMENTOS .

  refresh: TI_CONHEC.
  clear:   TI_CONHEC.

* Dados importados do Posto de Gasolina
  loop at TI_ZLEST0013.
    move: TI_ZLEST0013-CODTRP       to TI_CONHEC-CODTRP,
          TI_ZLEST0013-CODPOSTO     to TI_CONHEC-CODPOSTO,
          TI_ZLEST0013-LOTE         to TI_CONHEC-LOTE,
          TI_ZLEST0013-CTAFRETE     to TI_CONHEC-CTAFRETE,
          TI_ZLEST0013-CONHEC       to TI_CONHEC-CONHEC,
          TI_ZLEST0013-CHVID        to TI_CONHEC-CHVID,
          TI_ZLEST0013-DATALOTE     to TI_CONHEC-DATALOTE,
          TI_ZLEST0013-VLRLOTE      to TI_CONHEC-VLRLOTE,
          TI_ZLEST0013-VLRCONHEC    to TI_CONHEC-VLRCONHEC,
          TI_ZLEST0013-QTDE         to TI_CONHEC-QTDE,
          TI_ZLEST0013-DTACHEG      to TI_CONHEC-DTA_CHEGADA,
          TI_ZLEST0013-STATUS       to TI_CONHEC-STATUS,
          '13'                      to TI_CONHEC-ID_ORIGEM_ZLES.
    append TI_CONHEC.
    clear  TI_CONHEC.
  endloop.

* Dados A confirmar ou confirmados para Postos - Lotes
  clear: TI_ZLEST0015.

  loop at TI_ZLEST0016.

    if TI_ZLEST0015-TRANSPORTADOR <> TI_ZLEST0016-TRANSPORTADOR or
       TI_ZLEST0015-POSTO         <> TI_ZLEST0016-POSTO         or
       TI_ZLEST0015-LOTE          <> TI_ZLEST0016-LOTE.
      read table TI_ZLEST0015
        with key TRANSPORTADOR = TI_ZLEST0016-TRANSPORTADOR
                         POSTO = TI_ZLEST0016-POSTO
                          LOTE = TI_ZLEST0016-LOTE
      binary search.
      if not SY-SUBRC is initial.
        clear TI_ZLEST0015.
      endif.
    endif.

    move: TI_ZLEST0016-TRANSPORTADOR   to TI_CONHEC-CODTRP,
          TI_ZLEST0016-POSTO           to TI_CONHEC-CODPOSTO,
          TI_ZLEST0016-LOTE            to TI_CONHEC-LOTE,
          TI_ZLEST0016-CHVID           to TI_CONHEC-CHVID,
          TI_ZLEST0016-CTAFRETE        to TI_CONHEC-CTAFRETE,
          TI_ZLEST0016-CONHECIMENTO    to TI_CONHEC-CONHEC,
          TI_ZLEST0016-DTA_CHEGADA     to TI_CONHEC-DTA_CHEGADA,
          TI_ZLEST0015-STATUS          to TI_CONHEC-STATUS,
          '15'                         to TI_CONHEC-ID_ORIGEM_ZLES.
    append TI_CONHEC.
    clear  TI_CONHEC.
  endloop.

* Libera recursos
  free: TI_ZLEST0013.

endform.                    " YF_JUNCAO_CONHECIMENTOS

*&---------------------------------------------------------------------*
*&      Form  YF_CLASSIFICA_TABELAS_SELECOES
*&---------------------------------------------------------------------*
form YF_CLASSIFICA_TABELAS_SELECOES .

* Demais tabelas já foram classificadas nas devidas rotinas
  sort: TI_CONHEC     by CODTRP CODPOSTO LOTE STATUS CHVID
                         CONHEC CTAFRETE DATALOTE,
        TI_ZLEST0020  by TRANSPORTADOR POSTO LOTE CHVID CONHECIMENTO,
        TI_NFBALANCA  by DOCNUM,
        TI_TRANSPORTE by TDLNR EXTI1 EXTI2,
        TI_REMESSA    by TKNUM VBELN,
        TI_VBFA       by VBELV,
        TI_J_1BNFLIN  by REFKEY DOCNUM,
        TI_ZSDT0001   by VBELN,
        TI_VFKP       by REBEL FKNUM FKPOS,
        TI_KONV       by KNUMV KSCHL,
        TI_TOLERANCIA by TPLST BUKRS,
        TI_ZLEST0008  by LOTE.

endform.                    " YF_CLASSIFICA_TABELAS_SELECOES

*&---------------------------------------------------------------------*
*&      Form  YF_CARREGA_TABSAIDA_CONHEC
*&---------------------------------------------------------------------*
form YF_CARREGA_TABSAIDA_CONHEC .

  data: LQBR_CODTRP   type LIFNR,
        LQBR_CODPOSTO type LIFNR,
        LQBR_LOTE     type CHAR10,
        LQBR_STATUS   type ZTATUSLOTE,
        LI_TABIX      type I,
        LI_POS_INDEX  type I,
        LI_NRG_DTL    type I.
  refresh TI_LCTOCONFER.
  data: LC_PT_CONFER type ZLES_COCKPIT_CONFER_T,
        VL_BUKRS     like ZLEST0015-BUKRS,
        VL_DOCSAP    like ZLEST0015-DOCSAP,
        VL_GJAHR     like ZLEST0015-GJAHR,
        VL_MSG       type BAPI_MSG,
        VL_ERRO_DOC  type C1.
  clear: W_LOTE,
         W_LANCTO.
  field-symbols: <DATA> like line of LC_PT_CONFER.

  loop at TI_CONHEC into W_QBR_CONHEC.

    LI_TABIX = SY-TABIX.
    TI_CONHEC = W_QBR_CONHEC.

    if SY-TABIX = 1.
*     Inicializa controle de quebras
      LQBR_CODTRP    = TI_CONHEC-CODTRP.
      LQBR_CODPOSTO  = TI_CONHEC-CODPOSTO.
      LQBR_LOTE      = TI_CONHEC-LOTE.
      LQBR_STATUS    = TI_CONHEC-STATUS.
      W_OLD_CONHEC   = W_QBR_CONHEC.
      clear LI_NRG_DTL.
    endif.

*   Salva dados para lançamentos de conferência
    if TI_CONHEC-STATUS = CC_CONFIRMADO.
      TI_LCTOCONFER-CODTRP   = TI_CONHEC-CODTRP.
      TI_LCTOCONFER-CODPOSTO = TI_CONHEC-CODPOSTO.
      TI_LCTOCONFER-LOTE     = TI_CONHEC-LOTE.
      TI_LCTOCONFER-CHVID    = TI_CONHEC-CHVID.
      TI_LCTOCONFER-CONHEC   = TI_CONHEC-CONHEC.
      append TI_LCTOCONFER.
    endif.

*   Dados de detalhe do documento de conhecimento
    add 1 to LI_NRG_DTL.
    perform YF_ADICIONA_DTL_TABELA_LANCTO.

    if TI_CONHEC-CODTRP   <> LQBR_CODTRP   or
       TI_CONHEC-CODPOSTO <> LQBR_CODPOSTO or
       TI_CONHEC-LOTE     <> LQBR_LOTE     or
       TI_CONHEC-STATUS   <> LQBR_STATUS.

*     Dados de cabeçalho do documento de conhecimento
      LI_POS_INDEX = LI_TABIX - LI_NRG_DTL + 1.
      TI_CONHEC = W_OLD_CONHEC.
      perform YF_ADICIONA_HDR_TABELA_LOTE using LI_POS_INDEX.

*     Re-inicializa controle de quebra
      LI_NRG_DTL = 1.
      TI_CONHEC = W_QBR_CONHEC.
      LQBR_CODTRP   = TI_CONHEC-CODTRP.
      LQBR_CODPOSTO = TI_CONHEC-CODPOSTO.
      LQBR_LOTE     = TI_CONHEC-LOTE.
      LQBR_STATUS   = TI_CONHEC-STATUS.
      W_OLD_CONHEC   = W_QBR_CONHEC.
    endif.

  endloop.

  if LI_NRG_DTL > 0.
    LI_POS_INDEX = LI_TABIX - LI_NRG_DTL + 1.
    TI_CONHEC = W_OLD_CONHEC.
    perform YF_ADICIONA_HDR_TABELA_LOTE using LI_POS_INDEX.
  endif.

* Busca detalhes para lançamentos de Acréscimo/Decréscimo
  perform YF_ADICIONA_DTL_TABELA_ACRDECR tables <TI_LOTES>.

* Busca lançamentos de conferência
  check: not TI_LCTOCONFER[] is initial.
  sort TI_LCTOCONFER  by CODTRP CODPOSTO LOTE CHVID CONHEC.

  select TRANSPORTADOR POSTO LOTE CHVID
         TIPTRANSP CTLGLANCTO
         DOCSAP GJAHR BUKRS OBS_CONFER OBJ_KEY CONHECIMENTO
    into table <TI_CONFER>
    from ZLEST0022
     for all entries in TI_LCTOCONFER
   where TRANSPORTADOR = TI_LCTOCONFER-CODTRP
     and POSTO         = TI_LCTOCONFER-CODPOSTO
     and LOTE          = TI_LCTOCONFER-LOTE
     and CHVID         = TI_LCTOCONFER-CHVID
     and CONHECIMENTO  = TI_LCTOCONFER-CONHEC.

  loop at <TI_CONFER> assigning <DATA>.
    LI_TABIX = SY-TABIX.
*     Verificando se o documento de adto foi realmente criado ou nao.. e recuperando o numero(belnr) para a tabela de conferencia
    if ( <DATA>-DOCSAP is initial and <DATA>-OBJ_KEY is not initial ).

      clear: VL_DOCSAP, VL_BUKRS, VL_GJAHR.

*     Recuperando o documento contabil gerado..
      select single BELNR BUKRS GJAHR into (VL_DOCSAP, VL_BUKRS, VL_GJAHR)
        from ZIB_CONTABIL_CHV
       where OBJ_KEY = <DATA>-OBJ_KEY.

*     Analisa LOG de Erro na geração do documento contábil
      if VL_DOCSAP is initial.
*       Gera Log cockpit e libera o lote novamente se houver erro na criacao do doc contabil de conferencia..
*     Recuperando o erro do documento contabil gerado..
        select single MESSAGE into VL_MSG
          from ZIB_CONTABIL_ERR
         where OBJ_KEY = <DATA>-OBJ_KEY.
        if SY-SUBRC = 0.
          <DATA>-ERR_MSG = VL_MSG.
        endif.
      else.
*   Atualiza status do documento
        <DATA>-DOCSAP = VL_DOCSAP.
        <DATA>-GJAHR  = VL_GJAHR.
        update ZLEST0022
        set DOCSAP = VL_DOCSAP
            GJAHR  = VL_GJAHR
            BUKRS  = VL_BUKRS
        where TRANSPORTADOR = <DATA>-CODTRP
          and POSTO         = <DATA>-CODPOSTO
          and LOTE          = <DATA>-LOTE
          and OBJ_KEY       = <DATA>-OBJ_KEY.

        commit work and wait.
      endif.
    endif.
    clear: VL_DOCSAP, VL_BUKRS, VL_GJAHR, VL_MSG.
  endloop.


endform.                    " YF_CARREGA_TABSAIDA_CONHEC

*&---------------------------------------------------------------------*
*&      Form  YF_ADICIONA_DTL_TABELA_ACRDECR
*&---------------------------------------------------------------------*
form YF_ADICIONA_DTL_TABELA_ACRDECR tables PT_LOTE
                                    structure ZLES_COCKPIT_LOTE.

  data: LI_TABIX      type I,
        WA_DOCACRDCR  type TY_DOCACRDCR,
        WA_DOCACRDCR2 type TY_DOCACRDCR,
        VG_TABIX      type SY-TABIX,
        WA_ZLEST0022  type ZLEST0022.

  data: begin of LT_CHVACRDECR occurs 0,
          TRANSPORTADOR type TDLNR,
          POSTO         type LIFNR,
          LOTE          type CHAR10,
          CHVID         type ZCHVID,
          CONHECIMENTO   type EXTI1,
          CTAFRETE      type EXTI2,
          DOCSAP        type BELNR_D,
          GJAHR         type GJAHR,
          BUKRS         type BUKRS,
          ACDCTIPO      type ZACDCTIPO,
          ACDCINFO      type CHAR10,
        end   of LT_CHVACRDECR.

  check: not PT_LOTE[] is initial.

* Obtem documento de lançamento de acréscimo e decréscimo pendente
  select TRANSPORTADOR POSTO LOTE CHVID CONHECIMENTO CTAFRETE
         DOCSAP GJAHR BUKRS ACDCTIPO ACDCINFO
    into corresponding fields of table LT_CHVACRDECR
    from ZLEST0022
     for all entries in PT_LOTE
   where TRANSPORTADOR = PT_LOTE-CODTRP
     and POSTO         = PT_LOTE-CODPOSTO
     and ACDCTIPO    = CC_EXISTE_ACDC
*        OR
*         ( acdctipo    = cc_aplcdo_acdc   AND
*           acdcinfo    = pt_lote-lote ) )
    .

  check: not LT_CHVACRDECR[] is initial.

* Obtem detalhes do documento contábil - Item
  data ETL1076C2R8205 type table of BSEG.
  data LT_FIELDS_L1076C2R5208 type FAGL_T_FIELD.
  LT_FIELDS_L1076C2R5208 = value #( ( LINE = 'BUKRS' )
   ( LINE = 'BELNR' )
   ( LINE = 'GJAHR' )
   ( LINE = 'WRBTR' )
   ( LINE = 'SGTXT' )
   ).

  call function 'FAGL_GET_BSEG_FOR_ALL_ENTRIES'
    exporting
      IT_FOR_ALL_ENTRIES = LT_CHVACRDECR[]
      I_WHERE_CLAUSE     = |BUKRS = IT_FOR_ALL_ENTRIES-BUKRS AND BELNR = IT_FOR_ALL_ENTRIES-DOCSAP AND GJAHR = IT_FOR_ALL_ENTRIES-GJAHR AND SHKZG = { CL_ABAP_DYN_PRG=>QUOTE( CC_S ) }|
      IT_FIELDLIST       = LT_FIELDS_L1076C2R5208
    importing
      ET_BSEG            = ETL1076C2R8205
    exceptions
      NOT_FOUND          = 1.
  if SY-SUBRC = 0 and LINES( ETL1076C2R8205 ) > 0.
    clear TI_DOCACRDCR.
    types: begin of TYL1076C2R6472,
             BUKRS type BSEG-BUKRS,
             BELNR type BSEG-BELNR,
             GJAHR type BSEG-GJAHR,
             WRBTR type BSEG-WRBTR,
             SGTXT type BSEG-SGTXT,
           end of TYL1076C2R6472.
    data: LML1076C2R5638 type TYL1076C2R6472,
          LWL1076C2R1881 like line of TI_DOCACRDCR.
    loop at ETL1076C2R8205 reference into data(LDRL1076C2R4348).
      LML1076C2R5638-BUKRS = LDRL1076C2R4348->BUKRS.
      LML1076C2R5638-BELNR = LDRL1076C2R4348->BELNR.
      LML1076C2R5638-GJAHR = LDRL1076C2R4348->GJAHR.
      LML1076C2R5638-WRBTR = LDRL1076C2R4348->WRBTR.
      LML1076C2R5638-SGTXT = LDRL1076C2R4348->SGTXT.
      LWL1076C2R1881 = LML1076C2R5638.
      append LWL1076C2R1881 to TI_DOCACRDCR.
    endloop.
    SY-DBCNT = LINES( ETL1076C2R8205 ).
  else.
    SY-SUBRC = 4.
    SY-DBCNT = 0.
  endif.


* Obtem detalhes do documento contábil - Cabeçalho
  loop at TI_DOCACRDCR.
    LI_TABIX = SY-TABIX.
*   Tabela Cluster/Pool não pode participar do JOIN
    select single BUDAT WAERS
      into (TI_DOCACRDCR-BUDAT, TI_DOCACRDCR-WAERS)
      from BKPF
     where BUKRS eq TI_DOCACRDCR-BUKRS
       and BELNR eq TI_DOCACRDCR-BELNR
       and GJAHR eq TI_DOCACRDCR-GJAHR.
    check SY-SUBRC is initial.
    modify TI_DOCACRDCR index LI_TABIX
                 transporting BUDAT WAERS.
  endloop.

  loop at LT_CHVACRDECR where CHVID eq '26'.

    VG_TABIX = SY-TABIX.

    clear: WA_DOCACRDCR, WA_DOCACRDCR2.

    WA_DOCACRDCR-WRBTR = 0.

    select *
      into WA_ZLEST0022
      from ZLEST0022
     where TRANSPORTADOR eq LT_CHVACRDECR-TRANSPORTADOR
       and POSTO         eq LT_CHVACRDECR-POSTO
       and LOTE          eq LT_CHVACRDECR-LOTE
       and CHVID         eq '2'
       and CONHECIMENTO  eq LT_CHVACRDECR-CONHECIMENTO.

      "Obtem detalhes do documento contábil - Item
* ---> S4 Migration - 16/06/2023 - MA
*      SELECT SINGLE BUKRS BELNR GJAHR WRBTR SGTXT
*        INTO WA_DOCACRDCR2
*        FROM BSEG
*       WHERE BUKRS = WA_ZLEST0022-BUKRS
*         AND BELNR = WA_ZLEST0022-DOCSAP
*         AND GJAHR = WA_ZLEST0022-GJAHR
*         AND SHKZG = CC_S.

      data:   LT_BSEG type FAGL_T_BSEG.

      call function 'FAGL_GET_BSEG'
        exporting
          I_BUKRS   = WA_ZLEST0022-BUKRS
          I_BELNR   = WA_ZLEST0022-DOCSAP
          I_GJAHR   = WA_ZLEST0022-GJAHR
        importing
          ET_BSEG   = LT_BSEG
        exceptions
          NOT_FOUND = 1
          others    = 2.

      delete LT_BSEG where SHKZG ne CC_S.

      read table LT_BSEG into data(LS_BSEG) index 1.
      if SY-SUBRC = 0.
        move-corresponding LS_BSEG to WA_DOCACRDCR2.
      endif.
*<--- S4 Migration - 15/06/2023 - MA


      WA_DOCACRDCR-BUKRS = WA_DOCACRDCR2-BUKRS.
      WA_DOCACRDCR-BELNR = WA_DOCACRDCR2-BELNR.
      WA_DOCACRDCR-GJAHR = WA_DOCACRDCR2-GJAHR.
      case WA_ZLEST0022-CTLGLANCTO.
        when 'Q' or 'P'.
          subtract WA_DOCACRDCR2-WRBTR from WA_DOCACRDCR-WRBTR.
        when 'VC' or 'S'.
          add WA_DOCACRDCR2-WRBTR to WA_DOCACRDCR-WRBTR.
      endcase.
      WA_DOCACRDCR-SGTXT = WA_DOCACRDCR2-SGTXT.

    endselect.

    if WA_DOCACRDCR-WRBTR le 0.
      continue.
    endif.

    select single BUDAT WAERS
      into (WA_DOCACRDCR-BUDAT, WA_DOCACRDCR-WAERS)
      from BKPF
     where BUKRS eq WA_DOCACRDCR-BUKRS
       and BELNR eq WA_DOCACRDCR-BELNR
       and GJAHR eq WA_DOCACRDCR-GJAHR.

    if SY-SUBRC is not initial.
      continue.
    endif.

    LT_CHVACRDECR-BUKRS  = WA_DOCACRDCR-BUKRS.
    LT_CHVACRDECR-DOCSAP = WA_DOCACRDCR-BELNR.
    LT_CHVACRDECR-GJAHR  = WA_DOCACRDCR-GJAHR.
    modify LT_CHVACRDECR index VG_TABIX transporting BUKRS DOCSAP GJAHR.

    append WA_DOCACRDCR to TI_DOCACRDCR.

  endloop.

  loop at LT_CHVACRDECR where CHVID eq '28'.

    VG_TABIX = SY-TABIX.

    clear: WA_DOCACRDCR, WA_DOCACRDCR2.

    WA_DOCACRDCR-WRBTR = 0.

    select *
      into WA_ZLEST0022
      from ZLEST0022
     where TRANSPORTADOR eq LT_CHVACRDECR-TRANSPORTADOR
       and POSTO         eq LT_CHVACRDECR-POSTO
       and LOTE          eq LT_CHVACRDECR-LOTE
       and CHVID         eq '1'
       and CONHECIMENTO  eq LT_CHVACRDECR-CONHECIMENTO.

      "Obtem detalhes do documento contábil - Item
* ---> S4 Migration - 16/06/2023 - MA
*      SELECT SINGLE BUKRS BELNR GJAHR WRBTR SGTXT
*        INTO WA_DOCACRDCR2
*        FROM BSEG
*       WHERE BUKRS = WA_ZLEST0022-BUKRS
*         AND BELNR = WA_ZLEST0022-DOCSAP
*         AND GJAHR = WA_ZLEST0022-GJAHR
*         AND SHKZG = CC_S.

*      data:   LT_BSEG type FAGL_T_BSEG.

      call function 'FAGL_GET_BSEG'
        exporting
          I_BUKRS   = WA_ZLEST0022-BUKRS
          I_BELNR   = WA_ZLEST0022-DOCSAP
          I_GJAHR   = WA_ZLEST0022-GJAHR
        importing
          ET_BSEG   = LT_BSEG
        exceptions
          NOT_FOUND = 1
          others    = 2.

      delete LT_BSEG where SHKZG ne CC_S.

      read table LT_BSEG into LS_BSEG index 1.
      if SY-SUBRC = 0.
        move-corresponding LS_BSEG to WA_DOCACRDCR2.
      endif.
*<--- S4 Migration - 15/06/2023 - MA


      WA_DOCACRDCR-BUKRS = WA_DOCACRDCR2-BUKRS.
      WA_DOCACRDCR-BELNR = WA_DOCACRDCR2-BELNR.
      WA_DOCACRDCR-GJAHR = WA_DOCACRDCR2-GJAHR.
      WA_DOCACRDCR-WRBTR = WA_DOCACRDCR2-WRBTR + WA_DOCACRDCR-WRBTR.
      WA_DOCACRDCR-SGTXT = WA_DOCACRDCR2-SGTXT.

    endselect.

    if WA_DOCACRDCR-WRBTR le 0.
      continue.
    endif.

    select single BUDAT WAERS
      into (WA_DOCACRDCR-BUDAT, WA_DOCACRDCR-WAERS)
      from BKPF
     where BUKRS eq WA_DOCACRDCR-BUKRS
       and BELNR eq WA_DOCACRDCR-BELNR
       and GJAHR eq WA_DOCACRDCR-GJAHR.

    if SY-SUBRC is not initial.
      continue.
    endif.

    LT_CHVACRDECR-BUKRS  = WA_DOCACRDCR-BUKRS.
    LT_CHVACRDECR-DOCSAP = WA_DOCACRDCR-BELNR.
    LT_CHVACRDECR-GJAHR  = WA_DOCACRDCR-GJAHR.
    modify LT_CHVACRDECR index VG_TABIX transporting BUKRS DOCSAP GJAHR.

    append WA_DOCACRDCR to TI_DOCACRDCR.

  endloop.

  sort TI_DOCACRDCR by BUKRS BELNR GJAHR.

* Grava registro de Acréscimo/Decréscimo
  loop at LT_CHVACRDECR.
    read table TI_DOCACRDCR with key BUKRS = LT_CHVACRDECR-BUKRS
                                     BELNR = LT_CHVACRDECR-DOCSAP
                                     GJAHR = LT_CHVACRDECR-GJAHR
                            binary search.
    check SY-SUBRC is initial.
    move-corresponding: LT_CHVACRDECR  to W_ACRDCR,
                        TI_DOCACRDCR   to W_ACRDCR.
    move: LT_CHVACRDECR-TRANSPORTADOR  to W_ACRDCR-CODTRP,
          LT_CHVACRDECR-POSTO          to W_ACRDCR-CODPOSTO,
          LT_CHVACRDECR-CONHECIMENTO   to W_ACRDCR-CONHECIMENTO,
          LT_CHVACRDECR-CTAFRETE       to W_ACRDCR-CTAFRETE.
    read table TI_ZLEST0025 with key CHVID = W_ACRDCR-CHVID
                            binary search.
    move TI_ZLEST0025-CTLGCHAVID       to W_ACRDCR-CTLGCHAVID.
    append W_ACRDCR to <TI_ACRDCR>.
  endloop.

endform.                    " YF_ADICIONA_DTL_TABELA_ACRDECR

*&---------------------------------------------------------------------*
*&      Form  YF_ADICIONA_DTL_TABELA_LANCTO
*&---------------------------------------------------------------------*
form YF_ADICIONA_DTL_TABELA_LANCTO .

  data: LI_INDEX type I,
        LC_SUBRC type I.

* Controle chave de identificação de lote
  clear TI_ZLEST0025.
  read table TI_ZLEST0025 with key CHVID = TI_CONHEC-CHVID
                          binary search.

  if TI_CONHEC-ID_ORIGEM_ZLES = '13'. "Origem do arquivo

    move-corresponding TI_CONHEC  to W_LANCTO.
    move: TI_CONHEC-DTA_CHEGADA   to W_LANCTO-DTACHEG,
          TI_ZLEST0025-DESCHVID   to W_LANCTO-DESCHVID,
          TI_ZLEST0025-CTLGCHAVID to W_LANCTO-CTLGCHAVID,
          TI_ZLEST0025-LCTOCHVID  to W_LANCTO-LCTOCHVID,
          TI_CONHEC-VLRCONHEC     to W_LANCTO-VLRIMPORTADO.

*   Dados de Transporte (VTTK + ZLEST0024)
    read table TI_TRANSPORTE with key TDLNR = TI_CONHEC-CODTRP
                                      EXTI1 = TI_CONHEC-CONHEC
                                      EXTI2 = TI_CONHEC-CTAFRETE
                             binary search.
    LC_SUBRC = SY-SUBRC.

    if TI_ZLEST0025-CALCOCKPIT = CC_S.
      if LC_SUBRC is initial.
*       Obtem dados da Remessa
        perform YF_DADOS_ADIC_REMESSA_TRANSP using TI_CONHEC-VLRCONHEC.
      endif.
    else.
      if LC_SUBRC is initial.
        read table TI_VFKP with key REBEL = TI_TRANSPORTE-TKNUM
                           binary search.
        if SY-SUBRC is initial.
          read table TI_KONV with key KNUMV = TI_VFKP-KNUMV
                                      KSCHL = CC_KSCHL_ZFRE
                             binary search.
          if SY-SUBRC is initial.
            W_LANCTO-UNID_PESO = TI_KONV-KMEIN.
          endif.
        endif.
      endif.
      move W_LANCTO-VLRIMPORTADO to W_LANCTO-VLRPROGRAMADO.
      move W_LANCTO-VLRIMPORTADO to W_LANCTO-VLRCONFIRMADO.
    endif.

*   Determina a unidade de medida
    if W_LANCTO-UNID_PESO is initial.
      W_LANCTO-UNID_PESO = CC_UNID_PESO_DEFAULT.
    endif.

    W_LANCTO-PESO_IMPORTADO = TI_CONHEC-QTDE.
    perform YF_CONVERTE_VLR_UNID_PESO using CC_UNID_PESO_DEFAULT
                                            W_LANCTO-UNID_PESO
                                   changing W_LANCTO-PESO_IMPORTADO.
*   Anexa saida
    append W_LANCTO to <TI_LANCTOS>.

  else. "15 Origem do arquivo

*   Posto - Lotes (Header)
    read table TI_ZLEST0015 with key TRANSPORTADOR = TI_CONHEC-CODTRP
                                     POSTO         = TI_CONHEC-CODPOSTO
                                     LOTE          = TI_CONHEC-LOTE
                            binary search.

*   Posto - Lotes (Detalhes)
    read table TI_ZLEST0016
          with key TRANSPORTADOR = TI_CONHEC-CODTRP
                   POSTO         = TI_CONHEC-CODPOSTO
                   LOTE          = TI_CONHEC-LOTE
                   CHVID         = TI_CONHEC-CHVID
                   CONHECIMENTO  = TI_CONHEC-CONHEC
                   CTAFRETE      = TI_CONHEC-CTAFRETE
    binary search.
    check: SY-SUBRC is initial.

    move-corresponding TI_ZLEST0016     to W_LANCTO.
    move: TI_ZLEST0016-TRANSPORTADOR    to W_LANCTO-CODTRP,
          TI_ZLEST0016-POSTO            to W_LANCTO-CODPOSTO,
          TI_ZLEST0016-CONHECIMENTO     to W_LANCTO-CONHEC,
          TI_CONHEC-DTA_CHEGADA         to W_LANCTO-DTACHEG,
          TI_ZLEST0025-DESCHVID         to W_LANCTO-DESCHVID,
          TI_ZLEST0025-CTLGCHAVID       to W_LANCTO-CTLGCHAVID,
          TI_ZLEST0025-LCTOCHVID        to W_LANCTO-LCTOCHVID,
          TI_ZLEST0016-UNIDADE_PESO     to W_LANCTO-UNID_PESO,
          TI_ZLEST0016-VLR_ORIGEM       to W_LANCTO-VLRORIGEM,
          TI_ZLEST0016-VLR_IMPORTADO    to W_LANCTO-VLRIMPORTADO,
          TI_ZLEST0016-VLR_CONFIRMADO   to W_LANCTO-VLRCONFIRMADO,
          TI_ZLEST0016-DIFERENCA        to W_LANCTO-VLRDIFERENCA,
          TI_ZLEST0016-VLR_PROGRAMADO   to W_LANCTO-VLRPROGRAMADO,
          TI_ZLEST0016-PESO_CONFIRALTER to W_LANCTO-PESO_CONFIRALTER,
          TI_ZLEST0016-VLR_CONFIRALTER  to W_LANCTO-VLRCONFIRALTER.

    if TI_ZLEST0015-DATA  is initial.
      move TI_CONHEC-DATALOTE          to  W_LANCTO-DATALOTE.
    else.
      move TI_ZLEST0015-DATA           to W_LANCTO-DATALOTE.
    endif.

*   Anexa saida
    append W_LANCTO to <TI_LANCTOS>.

*   Dados Delta
    if TI_ZLEST0025-CALCOCKPIT = CC_S.

      read table TI_ZLEST0020
        with key TRANSPORTADOR = TI_ZLEST0016-TRANSPORTADOR
                         POSTO = TI_ZLEST0016-POSTO
                          LOTE = TI_ZLEST0016-LOTE
                         CHVID = TI_ZLEST0016-CHVID
                  CONHECIMENTO = TI_ZLEST0016-CONHECIMENTO
      binary search.

      if SY-SUBRC is initial.

        move-corresponding TI_ZLEST0020   to W_DELTA.
        move: TI_ZLEST0020-TRANSPORTADOR  to W_DELTA-CODTRP,
              TI_ZLEST0020-POSTO          to W_DELTA-CODPOSTO,
              W_LANCTO-DATALOTE           to W_DELTA-DATALOTE,
              W_LANCTO-CONHEC             to W_DELTA-CONHEC,
              W_LANCTO-CTAFRETE           to W_DELTA-CTAFRETE.

*       Anexa saida
        append W_DELTA to <TI_DELTAS>.

      endif.

    endif.

  endif.

* Limpa área de transferência
  clear: W_LANCTO,
         W_DELTA.

endform.                    " YF_ADICIONA_DTL_TABELA_LANCTO

*&---------------------------------------------------------------------*
*&      Form  YF_DADOS_ADIC_REMESSA_TRANSP
*&---------------------------------------------------------------------*
form YF_DADOS_ADIC_REMESSA_TRANSP using P_VLRCONHEC.

  data: LI_INDEX            type I,
        LI_INDEX2           type I,
        LQBR_VBELN          type VBELN_VL,
        LP_PESO_ORIGEM      type BRGEW_AP,
        LP_PESO_CONFIRM_ALL type BRGEW_AP,
        LP_PESO_HERMASA     type BRGEW_AP,
        LP_VLR_ALIQ_FRETE   type KBETR,
        LP_VLR_IOF_SEGURO   type KBETR,
        LP_VLR_VLRORIGEM    type KWERT,
        LP_VLR_IMPOSTOS     type KWERT,
        LP_VLR_NOTA_FISCAL  type KWERT,
        LP_VLR_ADIANTAMENTO type KWERT,
        LP_VLR_SEGURO       type KWERT.

* Valor de Origem
  read table TI_VFKP with key REBEL = TI_TRANSPORTE-TKNUM binary search.
  if SY-SUBRC is initial.
    LI_INDEX = SY-TABIX.
    do.
      LP_VLR_VLRORIGEM = LP_VLR_VLRORIGEM +
                         TI_VFKP-KZWI1. " + ti_vfkp-kzwi2.
      read table TI_KONV with key KNUMV = TI_VFKP-KNUMV
                         binary search.
      if SY-SUBRC is initial.
        LI_INDEX2 = SY-TABIX.
        do.
          if TI_KONV-KSCHL = CC_KSCHL_ZFRE.
*           Extrai a unidade de peso para relatório
            W_LANCTO-UNID_PESO = TI_KONV-KMEIN.
*           Soma valores  de frete
            LP_VLR_ALIQ_FRETE = LP_VLR_ALIQ_FRETE + TI_KONV-KBETR.
          elseif TI_KONV-KSCHL = CC_KSCHL_ZADM.
*           Soma adiantamento
            LP_VLR_ADIANTAMENTO = LP_VLR_ADIANTAMENTO + TI_KONV-KBETR.
          elseif TI_KONV-KSCHL = CC_KSCHL_ZSEG.
*           Soma valor de seguro
            LP_VLR_SEGURO = LP_VLR_SEGURO + TI_KONV-KWERT.
          else.
*           Soma valores de impostos
            LP_VLR_IMPOSTOS = LP_VLR_IMPOSTOS + TI_KONV-KWERT.
            if TI_KONV-KSCHL = CC_KSCHL_ZIOF.
*             Salva IOF para pessoa juridica
              LP_VLR_IOF_SEGURO = LP_VLR_IOF_SEGURO + TI_KONV-KWERT.
            endif.
          endif.

          add 1 to LI_INDEX2.
          read table TI_KONV index LI_INDEX2.
          if SY-SUBRC <> 0 or TI_KONV-KNUMV <> TI_VFKP-KNUMV.
            exit.
          endif.
        enddo.
      endif.

      add 1 to LI_INDEX.
      read table TI_VFKP index LI_INDEX.
      if SY-SUBRC <> 0 or TI_VFKP-REBEL <> TI_TRANSPORTE-TKNUM.
        exit.
      endif.
    enddo.
  endif.

* Converte para valores absoluto
  LP_VLR_IMPOSTOS = ABS( LP_VLR_IMPOSTOS ).

* Determina a unidade de medida a ser devolvida
  if W_LANCTO-UNID_PESO is initial.
    W_LANCTO-UNID_PESO = CC_UNID_PESO_DEFAULT.
  endif.

* Lê remessa
  read table TI_REMESSA with key TKNUM = TI_TRANSPORTE-TKNUM
                        binary search.
  if SY-SUBRC is initial.
    LI_INDEX = SY-TABIX.

*   Inicializa variável de quebra
    replace all occurrences of regex '\s' in LQBR_VBELN
          with CL_ABAP_CHAR_UTILITIES=>MAXCHAR.

*   Documento de transporte 1:N Remessas
    do.
*     Soma Peso de Origem na unidade default
      perform YF_CONVERTE_VLR_UNID_PESO using TI_REMESSA-GEWEI
                                              W_LANCTO-UNID_PESO
                                     changing TI_REMESSA-BTGEW.
      LP_PESO_ORIGEM = LP_PESO_ORIGEM + TI_REMESSA-BTGEW.

      if LQBR_VBELN <> TI_REMESSA-VBELN.
        LQBR_VBELN = TI_REMESSA-VBELN.
*       Soma Peso Confirmado ALL
        perform YF_SOMA_PESO_CONFIRMADO_ALL using TI_REMESSA-VBELN
                                                  W_LANCTO-UNID_PESO
                                         changing LP_PESO_CONFIRM_ALL
                                                  LP_VLR_NOTA_FISCAL.
*       Soma Peso Confirmado ALL
        perform YF_SOMA_PESO_HERMASA using TI_REMESSA-VBELN
                                           W_LANCTO-UNID_PESO
                                  changing LP_PESO_HERMASA.
      endif.

      add 1 to LI_INDEX.
      read table TI_REMESSA index LI_INDEX.
      if SY-SUBRC <> 0 or TI_REMESSA-TKNUM <> TI_TRANSPORTE-TKNUM.
        exit.
      endif.
    enddo.

  endif.

* Valor Confirmado
  clear TI_TOLERANCIA.
  read table TI_TOLERANCIA with key TPLST = TI_TRANSPORTE-TPLST
                           binary search.

  if not LP_PESO_CONFIRM_ALL is initial.
    W_LANCTO-PESO_CONFIRMADO = LP_PESO_CONFIRM_ALL.
  elseif not LP_PESO_HERMASA is initial.
    W_LANCTO-PESO_CONFIRMADO = LP_PESO_HERMASA.
  else.
    W_LANCTO-PESO_CONFIRMADO = TI_CONHEC-QTDE.
    perform YF_CONVERTE_VLR_UNID_PESO: using CC_UNID_PESO_DEFAULT
                                             W_LANCTO-UNID_PESO
                                    changing W_LANCTO-PESO_CONFIRMADO.
  endif.

* Soma IOF para seguro quando for pessoa Jurídica
  if LP_VLR_SEGURO > 0 and W_DELTA-GRPCONTA <> CC_ADD02_03.
    LP_VLR_SEGURO = LP_VLR_SEGURO + LP_VLR_IOF_SEGURO.
  endif.

* Valores base de cálculo
  clear W_DELTA.
  W_DELTA-CODTRP           = W_LANCTO-CODTRP.
  W_DELTA-CODPOSTO         = W_LANCTO-CODPOSTO.
  W_DELTA-LOTE             = W_LANCTO-LOTE.
  W_DELTA-DATALOTE         = W_LANCTO-DATALOTE.
  W_DELTA-CHVID            = W_LANCTO-CHVID.
  W_DELTA-CONHEC           = W_LANCTO-CONHEC.
  W_DELTA-CTAFRETE         = W_LANCTO-CTAFRETE.
  W_DELTA-VLRADIANTAMENTO  = LP_VLR_ADIANTAMENTO.
  W_DELTA-VLRIMP_RETIDOS   = LP_VLR_IMPOSTOS.
  W_DELTA-VLRSEGURO        = LP_VLR_SEGURO.
  W_DELTA-VLRFRETE         = LP_VLR_ALIQ_FRETE.
  W_LANCTO-VLRORIGEM       = LP_VLR_VLRORIGEM.
  W_LANCTO-PESO_ORIGEM     = LP_PESO_ORIGEM.
  W_DELTA-TOLERANCIA       = TI_TOLERANCIA-TOLERANCIA.
  W_DELTA-GRPCONTA         = TI_TRANSPORTE-ADD02.
  W_DELTA-UNID_PESO        = W_LANCTO-UNID_PESO.

* Calculo usado no cockpit
  perform YF_COCKPIT_CALCULO_DELTAS using W_LANCTO-PESO_CONFIRMADO
                                          LP_PESO_ORIGEM
                                          W_LANCTO-UNID_PESO
                                          W_DELTA-GRPCONTA
                                          W_DELTA-TOLERANCIA
                                          LP_VLR_NOTA_FISCAL
                                          LP_VLR_ADIANTAMENTO
                                          LP_VLR_IMPOSTOS
                                          LP_VLR_ALIQ_FRETE
                                          LP_VLR_SEGURO
                                          P_VLRCONHEC
                                 changing W_LANCTO-VLRCONFIRMADO
                                          W_LANCTO-VLRDIFERENCA
                                          W_LANCTO-VLRPROGRAMADO
                                          W_DELTA-VLCONFIRMADO_REF
                                          W_DELTA-VLRPERDA
                                          W_DELTA-VLRSOBRA_QUEBRA
                                          W_DELTA-DIFER_TRANSP
                                          W_DELTA-DIFERENCA_PESO
                                          W_DELTA-QUEBRA_PESO
                                          W_DELTA-QUEBRA_REAL
                                          W_DELTA-FATOR_CONVERSAO.

** Atualiza tabela de calculos - Valores Deltas
  append W_DELTA          to <TI_DELTAS>.

endform.                    " YF_DADOS_ADIC_REMESSA_TRANSP

*&---------------------------------------------------------------------*
*&      Form  YF_COCKPIT_CALCULO_DELTAS
*&---------------------------------------------------------------------*
form YF_COCKPIT_CALCULO_DELTAS  using P_PESO_CONFIRMADO
                                      P_PESO_ORIGEM
                                      P_UNID_PESO
                                      P_GRUPO_CONTA
                                      P_TOLERANCIA
                                      P_VLR_NOTA_FISCAL
                                      P_VLR_ADIANTAMENTO
                                      P_VLR_IMPOSTOS
                                      P_VLR_FRETE
                                      P_VLR_SEGURO
                                      P_VLR_IMPORTADO
                             changing S_VLR_CONFIRMADO
                                      S_VLR_DIFERENCA
                                      S_VLR_PROGRAMADO
                                      S_VLR_CONFIRM_REF
                                      S_VLR_PERDA
                                      S_VLR_SOBRA_QUEBRA
                                      S_DIFER_TRANSP
                                      S_DIFER_PESO
                                      S_QUEBRA_PESO
                                      S_QUEBRA_REAL
                                      S_FATOR_CONVERSAO.

  data: LP_PESO_ORIG_UNIDEF  type ZQUANT17_7,
          LP_PESO_CONF_DEFAULT type ZQUANT17_7,
          LP_QUEBRA_PESO       type ZQUANT17_7,
          LP_FATOR_CONVERS     type ZQUANT17_7,
          LP_DIFER_TRANSP      type ZQUANT17_7,
          LP_DIFER_PESO        type ZQUANT17_7,
          LP_QUEBRA_REAL       type ZQUANT17_7,
        LP_QUANTIDADE_AUX1   type ZQUANT17_7,
        LP_QUANTIDADE_AUX2   type ZQUANT17_7,
        LP_VLR_PERDA         type KWERT,
        LP_VLR_DIFER_CONF    type KWERT,
        LP_VLR_SOBRA_QUEBRA  type KWERT.

* Converte valores para unidade default
  LP_PESO_CONF_DEFAULT = P_PESO_CONFIRMADO.
  perform YF_CONVERTE_VLR_UNID_PESO using P_UNID_PESO
                                          CC_UNID_PESO_DEFAULT
                                 changing LP_PESO_CONF_DEFAULT.

  LP_PESO_ORIG_UNIDEF  = P_PESO_ORIGEM.
  perform YF_CONVERTE_VLR_UNID_PESO using P_UNID_PESO
                                          CC_UNID_PESO_DEFAULT
                                 changing LP_PESO_ORIG_UNIDEF.

* Calcula valores de peso na unidade default
  LP_DIFER_PESO  = LP_PESO_CONF_DEFAULT - LP_PESO_ORIG_UNIDEF.
  LP_QUEBRA_REAL = ( LP_PESO_ORIG_UNIDEF * P_TOLERANCIA ) / 100.
  LP_QUEBRA_PESO = LP_QUEBRA_REAL - ABS( LP_DIFER_PESO ).
*  IF lp_quebra_peso > 0.
  if LP_PESO_CONF_DEFAULT > LP_PESO_ORIG_UNIDEF or LP_QUEBRA_PESO > 0.
    LP_QUEBRA_PESO = 0.
  endif.
  if LP_PESO_ORIG_UNIDEF > 0.
    LP_FATOR_CONVERS = P_VLR_NOTA_FISCAL / LP_PESO_ORIG_UNIDEF.
  else.
    LP_FATOR_CONVERS = 0.
  endif.

  if LP_QUEBRA_PESO = 0.
    clear LP_DIFER_TRANSP.
  else.
    LP_DIFER_TRANSP  = LP_QUEBRA_PESO * LP_FATOR_CONVERS.
  endif.

* Calcula valores de moeda na unidade de peso
  if P_UNID_PESO = CC_UNID_PESO_DEFAULT.
    LP_VLR_DIFER_CONF   = P_VLR_FRETE * LP_PESO_CONF_DEFAULT.
    LP_VLR_SOBRA_QUEBRA = P_VLR_FRETE * LP_QUEBRA_PESO.
    LP_VLR_PERDA      = ( P_VLR_FRETE * LP_PESO_CONF_DEFAULT ) -
                        ( P_VLR_FRETE * LP_PESO_ORIG_UNIDEF  ).
  else.
    LP_QUANTIDADE_AUX1 = LP_PESO_CONF_DEFAULT.
    LP_QUANTIDADE_AUX2 = LP_QUEBRA_PESO.
    perform YF_CONVERTE_VLR_UNID_PESO using  CC_UNID_PESO_DEFAULT
                                             P_UNID_PESO
                                   changing: LP_QUANTIDADE_AUX1,
                                             LP_QUANTIDADE_AUX2.

    LP_VLR_DIFER_CONF   = P_VLR_FRETE * LP_QUANTIDADE_AUX1.
    LP_VLR_SOBRA_QUEBRA = P_VLR_FRETE * LP_QUANTIDADE_AUX2.

    LP_QUANTIDADE_AUX1 = LP_PESO_ORIG_UNIDEF.
    LP_QUANTIDADE_AUX2 = LP_PESO_CONF_DEFAULT.
    perform YF_CONVERTE_VLR_UNID_PESO using  CC_UNID_PESO_DEFAULT
                                             P_UNID_PESO
                                   changing: LP_QUANTIDADE_AUX1,
                                             LP_QUANTIDADE_AUX2.

    LP_VLR_PERDA      = ( P_VLR_FRETE * LP_QUANTIDADE_AUX2 ) -
                        ( P_VLR_FRETE * LP_QUANTIDADE_AUX1 ).
  endif.

* Atribui para tipo de parceiro
  if P_GRUPO_CONTA = CC_ADD02_03.
*   Pessoa Fisica
    S_VLR_CONFIRMADO = LP_VLR_DIFER_CONF - P_VLR_ADIANTAMENTO
                       - ABS( LP_DIFER_TRANSP ) - P_VLR_IMPOSTOS.
  else.
*   Pessoa Jurídica
    if ABS( LP_QUEBRA_PESO ) > 0.
      S_VLR_CONFIRMADO = LP_VLR_DIFER_CONF - P_VLR_ADIANTAMENTO
                       - ABS( LP_DIFER_TRANSP ).
    else.
      S_VLR_CONFIRMADO = LP_VLR_DIFER_CONF - P_VLR_ADIANTAMENTO.
    endif.
    subtract P_VLR_SEGURO from S_VLR_CONFIRMADO.
  endif.

  S_VLR_DIFERENCA = S_VLR_CONFIRMADO - P_VLR_IMPORTADO.
  if S_VLR_CONFIRMADO < P_VLR_IMPORTADO.
    S_VLR_PROGRAMADO = S_VLR_CONFIRMADO.
  else.
    S_VLR_PROGRAMADO = P_VLR_IMPORTADO.
  endif.

  S_DIFER_TRANSP     = LP_DIFER_TRANSP.
  S_VLR_CONFIRM_REF  = LP_VLR_DIFER_CONF.
  S_FATOR_CONVERSAO  = LP_FATOR_CONVERS.
  S_VLR_PERDA        = LP_VLR_PERDA.
  S_VLR_SOBRA_QUEBRA = LP_VLR_SOBRA_QUEBRA.

* Converte valores de unidade default de calculo peso p/unidade de peso
  perform YF_CONVERTE_VLR_UNID_PESO using CC_UNID_PESO_DEFAULT
                                          P_UNID_PESO
                                changing: LP_DIFER_PESO,
                                          LP_QUEBRA_PESO,
                                          LP_QUEBRA_REAL.
  S_DIFER_PESO   = LP_DIFER_PESO.
  S_QUEBRA_PESO  = LP_QUEBRA_PESO.
  S_QUEBRA_REAL  = LP_QUEBRA_REAL.

endform.                    " YF_COCKPIT_CALCULO_DELTAS

*&---------------------------------------------------------------------*
*&      Form  YF_SOMA_PESO_CONFIRMADO_ALL
*&---------------------------------------------------------------------*
form YF_SOMA_PESO_CONFIRMADO_ALL  using P_VBELN
                                        P_UNID_CONVERSAO
                               changing P_PESO_CONFIRM_ALL
                                        P_VLR_NOTA_FISCAL.
  data: LI_INDEX      type I,
        LP_PESO_CHEGA type BRGEW_AP.


* Lê fluxo de documento de faturamento
  read table TI_VBFA with key VBELV = TI_REMESSA-VBELN binary search.
  check: SY-SUBRC is initial.

* Lê documento de NF para documento de faturamento
  read table TI_J_1BNFLIN with key REFKEY = TI_VBFA-REFKEY
                          binary search.
  check: SY-SUBRC is initial.
  LI_INDEX = SY-TABIX.

  do.
*   Lê documento de NF e dados da Balança
    read table TI_NFBALANCA
         with key DOCNUM = TI_J_1BNFLIN-DOCNUM binary search.

    if SY-SUBRC is initial and TI_NFBALANCA-PESO_CHEGA > 0.

      LP_PESO_CHEGA = TI_NFBALANCA-PESO_CHEGA.

      perform YF_CONVERTE_VLR_UNID_PESO using TI_NFBALANCA-GEWEI
                                              P_UNID_CONVERSAO
                                     changing LP_PESO_CHEGA.

      P_PESO_CONFIRM_ALL = P_PESO_CONFIRM_ALL +  LP_PESO_CHEGA.
    endif.

*   Acumula Valor da nota fiscal
    perform YF_ACUMULA_VLR_NOTA_FISCAL using TI_J_1BNFLIN-DOCNUM
                                    changing P_VLR_NOTA_FISCAL.

    add 1 to LI_INDEX.
    read table TI_J_1BNFLIN index LI_INDEX.
    if SY-SUBRC <> 0 or TI_J_1BNFLIN-REFKEY <> TI_VBFA-REFKEY.
      exit.
    endif.

  enddo.

endform.                    " YF_SOMA_PESO_CONFIRMADO_ALL

*&---------------------------------------------------------------------*
*&      Form  YF_ACUMULA_VLR_NOTA_FISCAL
*&---------------------------------------------------------------------*
form YF_ACUMULA_VLR_NOTA_FISCAL  using P_DOCNUM
                              changing P_VLR_NOTA_FISCAL.

  statics: LC_DOCNUM          type J_1BDOCNUM,
           LP_VLR_NOTA_FISCAL type J_1BNFTOT.

  if LC_DOCNUM = P_DOCNUM.
    P_VLR_NOTA_FISCAL = P_VLR_NOTA_FISCAL + LP_VLR_NOTA_FISCAL.
    exit.
  else.
    LC_DOCNUM = P_DOCNUM.
    clear LP_VLR_NOTA_FISCAL.
  endif.

  clear W_DOCHEADER.

  call function 'J_1B_NF_DOCUMENT_READ'
    exporting
      DOC_NUMBER         = P_DOCNUM
    importing
      DOC_HEADER         = W_DOCHEADER
    tables
      DOC_PARTNER        = TI_DOCPARTNER
      DOC_ITEM           = TI_DOCITEM
      DOC_ITEM_TAX       = TI_DOCITEMTAX
      DOC_HEADER_MSG     = TI_DOCHEADERMSG
      DOC_REFER_MSG      = TI_DOCREFERMSG
    exceptions
      DOCUMENT_NOT_FOUND = 1
      DOCUM_LOCK         = 2
      others             = 3.

  check: not TI_DOCITEM[] is initial
    and  not TI_DOCITEMTAX[] is initial.

  clear: W_EXT_HEADER.

  call function 'J_1B_NF_VALUE_DETERMINATION'
    exporting
      NF_HEADER   = W_DOCHEADER
    importing
      EXT_HEADER  = W_EXT_HEADER
    tables
      NF_ITEM     = TI_DOCITEM
      NF_ITEM_TAX = TI_DOCITEMTAX
      EXT_ITEM    = TI_EXT_ITEM.

  loop at TI_EXT_ITEM.
    P_VLR_NOTA_FISCAL = P_VLR_NOTA_FISCAL + TI_EXT_ITEM-NFTOT.
    LP_VLR_NOTA_FISCAL = LP_VLR_NOTA_FISCAL + TI_EXT_ITEM-NFTOT.
  endloop.

endform.                    " YF_ACUMULA_VLR_NOTA_FISCAL

*&---------------------------------------------------------------------*
*&      Form  YF_SOMA_PESO_HERMASA
*&---------------------------------------------------------------------*
form YF_SOMA_PESO_HERMASA  using P_VBELN
                                 P_UNID_CONVERSAO
                        changing P_PESO_HERMASA.

  data: LI_INDEX  type I.

  read table TI_ZSDT0001 with key VBELN = P_VBELN
                         binary search.
  check: SY-SUBRC is initial.
  LI_INDEX = SY-TABIX.

  do.

*   Assume que nas tabelas "Z" a unidade esteja na unidade default
    perform YF_CONVERTE_VLR_UNID_PESO using CC_UNID_PESO_DEFAULT
                                            P_UNID_CONVERSAO
                                   changing TI_ZSDT0001-PESO_LIQ.

    P_PESO_HERMASA = P_PESO_HERMASA + TI_ZSDT0001-PESO_LIQ.

    add 1 to LI_INDEX.
    read table TI_ZSDT0001 index LI_INDEX.
    if SY-SUBRC <> 0 or TI_ZSDT0001-VBELN <> P_VBELN.
      exit.
    endif.

  enddo.

endform.                    " YF_SOMA_PESO_HERMASA


*&---------------------------------------------------------------------*
*&      Form  libera_lote_apos_erro_adto - Copia do metodo - IF_EX_AC_QUANTITY_GET~EXIT_QUANTITY_CHANGE da classe - ZCL_IM_CL_LES_ESTORNO_ADT
*&---------------------------------------------------------------------*
form LIBERA_LOTE_APOS_ERRO_ADTO.

  constants:
    C_IMPORTADO value 'I',
    CC_1_ACDC   value '1',
    CC_3_ACDC   value '3'.

  data: TI_ZLEST0013    type table of ZLEST0013,
        TI_ZLEST0016    type table of ZLEST0016,

        LW_ZLEST0013    type ZLEST0013,
        LW_ZLEST0015    type ZLEST0015,
        LW_ZLEST0016    type ZLEST0016,
        LN_NRINTEIRO(7) type N,
        LN_DECIMAL(3)   type N.

*----------------------------------------------------------------------
* Lançamentos associados ao lote
*----------------------------------------------------------------------
  select *
    into table TI_ZLEST0016
    from ZLEST0016
   where TRANSPORTADOR = TI_ZLEST0015-TRANSPORTADOR
     and POSTO         = TI_ZLEST0015-POSTO
     and LOTE          = TI_ZLEST0015-LOTE.

  check not TI_ZLEST0016 is initial.

*----------------------------------------------------------------------
* Dados gerais do Lote/Lançamento envolvidos no estorno
*----------------------------------------------------------------------
  clear LW_ZLEST0013.

* CNPJ da transportadora
  select single STCD1
    into LW_ZLEST0013-CNPJ_TRP
    from LFA1
   where LIFNR = TI_ZLEST0015-TRANSPORTADOR.

* CNPJ do posto
  select single STCD1
    into LW_ZLEST0013-CNPJ_POSTO
    from LFA1
   where LIFNR = TI_ZLEST0015-POSTO.

  LW_ZLEST0013-LOTE     = TI_ZLEST0015-LOTE.
  LW_ZLEST0013-CODTRP   = TI_ZLEST0015-TRANSPORTADOR.
  LW_ZLEST0013-CODPOSTO = TI_ZLEST0015-POSTO.
  LW_ZLEST0013-DATALOTE = TI_ZLEST0015-DATA.
  LW_ZLEST0013-VLRLOTE  = TI_ZLEST0015-VLR_IMPORTADO.
  LW_ZLEST0013-STATUS   = C_IMPORTADO.
  LW_ZLEST0013-DATA     = SY-DATUM.
  LW_ZLEST0013-HORA     = SY-UZEIT.
  LW_ZLEST0013-USUARIO  = SY-UNAME.

  loop at TI_ZLEST0016 into LW_ZLEST0016.
    LW_ZLEST0013-CTAFRETE   = LW_ZLEST0016-CTAFRETE.
    LW_ZLEST0013-CONHEC     = LW_ZLEST0016-CONHECIMENTO.
    LW_ZLEST0013-CHVID      = LW_ZLEST0016-CHVID.
    LW_ZLEST0013-VLRCONHEC  = LW_ZLEST0016-VLR_IMPORTADO.
    LW_ZLEST0013-DTACHEG    = LW_ZLEST0016-DTA_CHEGADA.
    LN_NRINTEIRO = TRUNC( LW_ZLEST0016-PESO_IMPORTADO ).
    LN_DECIMAL   = ( LW_ZLEST0016-VLR_IMPORTADO - LN_NRINTEIRO )
                 * 1000.
    concatenate LN_NRINTEIRO LN_DECIMAL into LW_ZLEST0013-QTDE.
    append LW_ZLEST0013 to TI_ZLEST0013.
  endloop.

*----------------------------------------------------------------------
* Atualiza base de seleção de importação
*----------------------------------------------------------------------
  modify ZLEST0013 from table TI_ZLEST0013.

*----------------------------------------------------------------------
* Equaliza documento para manutenção na bade lote/lançamentos
*----------------------------------------------------------------------
  sort TI_ZLEST0013 by CODTRP CODPOSTO LOTE.
  delete adjacent duplicates from TI_ZLEST0013
                  comparing CODTRP CODPOSTO LOTE.

*----------------------------------------------------------------------
* Elimina entradas da base lote e lançamentos a confirmar
*----------------------------------------------------------------------
  loop at TI_ZLEST0013 into LW_ZLEST0013.
*   Elimina lote
    delete
      from ZLEST0015
     where TRANSPORTADOR = LW_ZLEST0013-CODTRP
       and POSTO         = LW_ZLEST0013-CODPOSTO
       and LOTE          = LW_ZLEST0013-LOTE.
*   Elimina lançamento(S) do lote
    delete
      from ZLEST0016
     where TRANSPORTADOR = LW_ZLEST0013-CODTRP
       and POSTO         = LW_ZLEST0013-CODPOSTO
       and LOTE          = LW_ZLEST0013-LOTE.

*   Verifica acréscimo/decréscimo aplicado
*   Não é possível restaurar o rejeitado a menos que seja por data
    select count( * )
      from ZLEST0022
     where TRANSPORTADOR = LW_ZLEST0013-CODTRP
       and POSTO         = LW_ZLEST0013-CODPOSTO
       and ACDCTIPO      = CC_3_ACDC
       and ACDCINFO      = LW_ZLEST0013-LOTE.

    if SY-SUBRC is initial.

*     Altera acréscimo/decréscimo
      update ZLEST0022
         set ACDCTIPO = CC_1_ACDC
             ACDCINFO = SPACE
       where TRANSPORTADOR = LW_ZLEST0013-CODTRP
         and POSTO         = LW_ZLEST0013-CODPOSTO
         and ACDCTIPO      = CC_3_ACDC
         and ACDCINFO      = LW_ZLEST0013-LOTE.

      commit work and wait.
    endif.

  endloop.
endform.                    " libera_lote_apos_erro_adto

*&---------------------------------------------------------------------*
*&      Form  LANCTO_GERA_LOGLACTO_COCKPIT - Copia do include - LZLES0001F01
*&---------------------------------------------------------------------*
form LANCTO_GERA_LOGLACTO_COCKPIT using P_ERRO_ADTO.

  data: LT_LOG_ZIB  type standard table of ZIB_CONTABIL_ERR
                         with header line initial size 0.

  data: LC_CHAVE         type EPSFILNAM,
        LN_IDCTRL        type ZIDCTRL,
        LN_VCONT         type NUMC10,
        LD_DATA_CONTABIL type D,
        LC_DATA_CHAR     type CHAR10.

* Gera data para pesquisa no log (Um dia antes da data atual)
  perform YF_CALC_DATE using SY-DATUM '01' '00' '00' '-'
                    changing LD_DATA_CONTABIL.

* Obtem registros de log contábil
  select *
    into table LT_LOG_ZIB
    from ZIB_CONTABIL_ERR
   where OBJ_KEY = TI_ZLEST0015-OBJ_KEY.

* Gera controle para última chave de erro no log
  LC_CHAVE = SY-REPID.

  select max( IDCTRL ) max( CONT )
    into (LN_IDCTRL, LN_VCONT)
    from ZLEST0008
   where FILENAME = LC_CHAVE
    group by IDCTRL.
  endselect.

* Gera sequencia de controle de erro
  if SY-SUBRC is initial.
    if LN_VCONT >= '9999999000'.
      add 1 to LN_IDCTRL.
      clear LN_VCONT.
    endif.
  else.
    add 1 to LN_IDCTRL.
  endif.

* Transfere o erro para area de LOG
  clear ZLEST0008.

  ZLEST0008-FILENAME = LC_CHAVE.
  ZLEST0008-IDCTRL   = LN_IDCTRL.
  ZLEST0008-TCODE    = SY-TCODE.
  ZLEST0008-CONT     = LN_VCONT.
  ZLEST0008-MSGTYP   = CC_E.
  ZLEST0008-MSGSPRA  = SY-LANGU.
  ZLEST0008-MSGID    = 'FR'.
  ZLEST0008-MSGNR    = '999'.
  ZLEST0008-DATA     = SY-DATUM.
  ZLEST0008-HORA     = SY-UZEIT.
  ZLEST0008-USUARIO  = SY-UNAME.
  ZLEST0008-LOTE     = TI_ZLEST0015-LOTE.

  add 1 to LN_VCONT.
  ZLEST0008-CONT  = LN_VCONT.
  write SY-DATUM to LC_DATA_CHAR.

  if LT_LOG_ZIB[] is not initial.
    loop at LT_LOG_ZIB.
      add 1 to LN_VCONT.
      ZLEST0008-CONT  = LN_VCONT.
      concatenate LC_DATA_CHAR ': ' text-M09 ', ' LT_LOG_ZIB-MESSAGE
           into ZLEST0008-MSGV1.
      "      zlest0008-msgv1 = lt_log_zib-message.
      insert ZLEST0008.
    endloop.
  endif.
*     Retorna o lote para I - Importado para gerar novo documento de adto..
*     Libera lote para processamento novamente.. Status = I
  if LT_LOG_ZIB[] is not initial.
    perform LIBERA_LOTE_APOS_ERRO_ADTO.
    P_ERRO_ADTO = 'X'.
  endif.

endform.                    " LANCTO_GERA_LOGLACTO_COCKPIT

*&---------------------------------------------------------------------*
*&      Form  YF_ADICIONA_HDR_TABELA_LOTE
*&---------------------------------------------------------------------*
form YF_ADICIONA_HDR_TABELA_LOTE using P_DTL_INDEX.

  move: TI_CONHEC-CODTRP       to  W_LOTE-CODTRP,
        TI_CONHEC-CODPOSTO     to  W_LOTE-CODPOSTO,
        TI_CONHEC-LOTE         to  W_LOTE-LOTE,
        TI_CONHEC-STATUS       to  W_LOTE-STATUS,
        TI_CONHEC-CHVID        to  W_LOTE-CHVID,
        TI_CONHEC-CONHEC       to  W_LOTE-CONHEC,
        TI_CONHEC-CTAFRETE     to  W_LOTE-CTAFRETE,
        TI_CONHEC-DTA_CHEGADA  to  W_LOTE-DTACHEG.
* Controle chave de identificação de lote
  read table TI_ZLEST0025 with key CHVID = TI_CONHEC-CHVID
                          binary search.
  if SY-SUBRC is initial.
    move TI_ZLEST0025-DESCHVID to W_LOTE-DESCHVID.
  endif.

* Dados da Transportadora e Posto (LFA1)
  read table TI_TRANSPOSTO with key LIFNR = TI_CONHEC-CODTRP
                           binary search.
  if SY-SUBRC is initial.
    W_LOTE-DSCODTRP = TI_TRANSPOSTO-NAME1.
    W_LOTE-CNPJ_TRP = TI_TRANSPOSTO-CNPJ.
  endif.
  read table TI_TRANSPOSTO with key LIFNR = TI_CONHEC-CODPOSTO
                           binary search.
  if SY-SUBRC is initial.
    W_LOTE-DSCODPOSTO = TI_TRANSPOSTO-NAME1.
    W_LOTE-CNPJ_POSTO = TI_TRANSPOSTO-CNPJ.
  endif.

* Analisa o status do Documento
  if TI_CONHEC-ID_ORIGEM_ZLES = '13'.  "Status = I.

*   Dados acumulados de itens do conhecimento
    perform YF_OBTEM_VLRITENS_ACUMULADOS  using P_DTL_INDEX
                                       changing W_LOTE-VLRIMPORTADO
                                                W_LOTE-VLRCONFIRMADO
                                                W_LOTE-VLRORIGEM
                                                W_LOTE-VLRDIFERENCA
                                                W_LOTE-VLRREALIZADO
                                                W_LOTE-VLR_ACREC_DESC
                                                W_LOTE-VLR_A_PAGAR.

    check W_LOTE-VLR_A_PAGAR gt 0.

    W_LOTE-BL = CC_1.
    W_LOTE-DATALOTE = TI_CONHEC-DATALOTE.

    perform YF_CALC_DIAS_VENCIMENTO using VG_DIAS.

    perform YF_CALC_DATA_DIA_UTIL using TI_CONHEC-CODTRP
                                        TI_CONHEC-DATALOTE
                                        VG_DIAS
                               changing W_LOTE-VENCIMENTO.

    W_LOTE-VLRRECUSADO = W_LOTE-VLRIMPORTADO - W_LOTE-VLRCONFIRMADO.
    W_LOTE-ICON_STATUS = ICON_RED_LIGHT.

  else.   "Status = A e C
*   Obrigatório entradas para estes status - Posto - Lotes (ZLEST0015)
    read table TI_ZLEST0015 with key TRANSPORTADOR = W_LOTE-CODTRP
                                     POSTO         = W_LOTE-CODPOSTO
                                     LOTE          = W_LOTE-LOTE
                                     STATUS        = W_LOTE-STATUS
                            binary search.
    check: SY-SUBRC is initial.

    W_LOTE-DATALOTE       = TI_ZLEST0015-DATA.
    W_LOTE-DOCSAPADTO     = TI_ZLEST0015-DOCSAP.
    W_LOTE-VLRORIGEM      = TI_ZLEST0015-VLR_ORIGEM.
    W_LOTE-VLRDIFERENCA   = TI_ZLEST0015-DIFERENCA.
    W_LOTE-BL             = CC_2.
    W_LOTE-VENCIMENTO     = TI_ZLEST0015-VENCIMENTO.
    W_LOTE-VLRIMPORTADO   = TI_ZLEST0015-VLR_IMPORTADO.
    W_LOTE-VLRCONFIRMADO  = TI_ZLEST0015-VLR_CONFIRMADO.
    W_LOTE-VLRRECUSADO    = TI_ZLEST0015-VLR_RECUSADO.
    W_LOTE-VLRREALIZADO   = TI_ZLEST0015-VLR_REALIZADO.
    W_LOTE-DATAFECHAMENTO = TI_ZLEST0015-DATAFECHAMENTO.
    W_LOTE-VLR_ACREC_DESC = TI_ZLEST0015-VLR_ACREC_DESC.
    W_LOTE-VLR_A_PAGAR    = TI_ZLEST0015-VLR_A_PAGAR.

    if W_LOTE-STATUS = CC_A.
      W_LOTE-ICON_STATUS   = ICON_YELLOW_LIGHT.
    else.
      W_LOTE-ICON_STATUS   = ICON_GREEN_LIGHT.
    endif.

  endif.

* Log de erros LES (ZLEST0008)
  perform YF_CHECK_LOG_LES0008  using W_LOTE-LOTE
                             changing  W_LOTE-ICON_LOG.
* Anexa saida
  append W_LOTE to <TI_LOTES>.
  clear W_LOTE.

endform.                    " YF_ADICIONA_HDR_TABELA_LOTE

*&---------------------------------------------------------------------*
*&      Form  YF_CALC_DATA_DIA_UTIL
*&---------------------------------------------------------------------*
form YF_CALC_DATA_DIA_UTIL using P_CODTRP
                                 P_DATA_INICIO
                                 P_DIA
                        changing P_DATA_CALCULADA.

  data: LC_FABKL type FABKL,
        LP_FKDAY type FKDAY.

  clear P_DATA_CALCULADA.

* Busca o centro de um documento de transporte
  read table TI_TRANSPORTE with key TDLNR = P_CODTRP
                           binary search.
  if SY-SUBRC is initial.
    read table TI_VFKP with key REBEL = TI_TRANSPORTE-TKNUM binary search.
    if SY-SUBRC is initial.
      LC_FABKL = 'ZF'.  "Calendário default
**     Busca o calendário de fábrica
*      SELECT SINGLE fabkl
*        INTO lc_fabkl
*         FROM t001w
*         WHERE werks = ti_vfkp-werks.
    endif.
  endif.

  LP_FKDAY = P_DIA.
  LC_FABKL = 'ZF'.  "Calendário default

* Obtem o dia útil com o acrescimo informado
  call function 'WDKAL_DATE_ADD_FKDAYS'
    exporting
      I_DATE  = P_DATA_INICIO
      I_FKDAY = LP_FKDAY
      I_FABKL = LC_FABKL
    importing
      E_DATE  = P_DATA_CALCULADA
    exceptions
      ERROR   = 1
      others  = 2.

  check: SY-SUBRC <> 0.

  perform YF_CALC_DATE using TI_CONHEC-DATALOTE P_DIA '00' '00' '+'
                      changing P_DATA_CALCULADA.

endform.                    " YF_CALC_DATA_DIA_UTIL

*&---------------------------------------------------------------------*
*&      Form  YF_CALC_DATE
*&---------------------------------------------------------------------*
form YF_CALC_DATE     using P_DATA_INICIO
                            P_DIA
                            P_MES
                            P_ANO
                            P_SINAL
                   changing P_DATA_CALCULADA.

  data: LN_DIA   type DLYDY,
        LN_MES   type DLYMO,
        LN_ANO   type DLYYR,
        LC_SINAL type SPLI1.

  call function 'RP_CALC_DATE_IN_INTERVAL'
    exporting
      DATE      = P_DATA_INICIO
      DAYS      = LN_DIA
      MONTHS    = LN_MES
      SIGNUM    = LC_SINAL
      YEARS     = LN_ANO
    importing
      CALC_DATE = P_DATA_CALCULADA.

endform.                    " YF_CALC_DATE

*&---------------------------------------------------------------------*
*&      Form  YF_OBTEM_VLRITENS_ACUMULADOS
*&---------------------------------------------------------------------*
form YF_OBTEM_VLRITENS_ACUMULADOS  using P_INDEX
                                changing P_VLRIMPORTADO
                                         P_VLRCONFIRMADO
                                         P_VLRORIGEM
                                         P_VLRDIFERENCA
                                         P_VLRREALIZADO
                                         P_VLR_ACREC_DESC
                                         P_VLR_A_PAGAR.

  data: LW_LANCTO     type ZLES_COCKPIT_LANCTO,
        VL_CNPJ_TRP   type LFA1-STCD1,
        VL_CNPJ_POSTO type LFA1-STCD1,
        LV_VCONT      type ZLEST0008-CONT,
        LV_VERSION    type ZLEST0008-IDCTRL,
        WA_ZLEST0008  type ZLEST0008,
        WA_MSG        type BAPIRET2.

  check: P_INDEX > 0.

  clear: P_VLRIMPORTADO,
         P_VLRCONFIRMADO,
         P_VLRORIGEM,
         P_VLRDIFERENCA,
         P_VLRREALIZADO,
         P_VLR_ACREC_DESC,
         P_VLR_A_PAGAR.

  P_VLRIMPORTADO  = TI_CONHEC-VLRLOTE.

  loop at <TI_LANCTOS> into LW_LANCTO from P_INDEX.

    check: LW_LANCTO-CODTRP   = TI_CONHEC-CODTRP
       and LW_LANCTO-CODPOSTO = TI_CONHEC-CODPOSTO
       and LW_LANCTO-LOTE     = TI_CONHEC-LOTE .


    if LW_LANCTO-VLRPROGRAMADO le 0 .

      select single STCD1
        from LFA1
        into VL_CNPJ_TRP
       where LIFNR eq LW_LANCTO-CODTRP.

      select single STCD1
        from LFA1
        into VL_CNPJ_POSTO
       where LIFNR eq LW_LANCTO-CODPOSTO.

      delete
        from ZLEST0013
       where CNPJ_TRP   eq VL_CNPJ_TRP
         and CNPJ_POSTO eq VL_CNPJ_POSTO
         and LOTE       eq LW_LANCTO-LOTE
         and CTAFRETE   eq LW_LANCTO-CTAFRETE
         and CONHEC     eq LW_LANCTO-CONHEC
         and CHVID      eq LW_LANCTO-CHVID.

      "--Gerar Log
      clear: WA_ZLEST0008.

      select single max( IDCTRL ) max( CONT )
        into (WA_ZLEST0008-IDCTRL, LV_VCONT)
        from ZLEST0008
       where FILENAME = 'SAPLZLES0002'
       group by IDCTRL.

      if SY-SUBRC is initial.
        if LV_VCONT >= '9999999998'.
          LV_VERSION  = WA_ZLEST0008-IDCTRL + 1.
          LV_VCONT    = 1.
        else.
          LV_VERSION = WA_ZLEST0008-IDCTRL.
          LV_VCONT   = LV_VCONT + 1.
        endif.
      else.
        LV_VERSION = WA_ZLEST0008-IDCTRL + 1.
      endif.

      WA_ZLEST0008-FILENAME  = 'SAPLZLES0002'.
      WA_ZLEST0008-IDCTRL    = LV_VERSION.
      WA_ZLEST0008-TCODE     = 'ZLES0031'.
      WA_ZLEST0008-CONT      = LV_VCONT.
      WA_ZLEST0008-MSGTYP    = 'W'.
      WA_ZLEST0008-MSGSPRA   = 'PT'.
      WA_ZLEST0008-MSGID     = 'FR'.
      WA_ZLEST0008-MSGNR     = '999'.
      concatenate 'LANÇAMENTO COM VALOR PROGRAMADO NEGATIVO EXCLUÍDO CTA. FRETE:' LW_LANCTO-CTAFRETE ' - CONHEC.:' LW_LANCTO-CONHEC  into WA_ZLEST0008-MSGV1.
      WA_ZLEST0008-DATA      = SY-DATUM.
      WA_ZLEST0008-HORA      = SY-UZEIT.
      WA_ZLEST0008-USUARIO   = SY-UNAME.
      WA_ZLEST0008-LOTE      = LW_LANCTO-LOTE.

      WA_MSG-TYPE        = WA_ZLEST0008-MSGTYP.
      WA_MSG-ID          = WA_ZLEST0008-MSGID.
      WA_MSG-NUMBER      = WA_ZLEST0008-MSGNR.
      WA_MSG-MESSAGE     = WA_ZLEST0008-MSGV1.
      WA_MSG-LOG_NO      = LV_VERSION.
      WA_MSG-SYSTEM      = 'SAPLZLES0002'.
*      wa_msg-LOG_MSG_NO  = .
*      wa_msg-MESSAGE_V1  = .
*      wa_msg-MESSAGE_V2  = .
*      wa_msg-MESSAGE_V3  = .
*      wa_msg-MESSAGE_V4  = .
*      wa_msg-PARAMETER   = .
*      wa_msg-ROW         = .
*      wa_msg-FIELD       = .


      append WA_MSG to <TI_MSG>.

      delete  <TI_LANCTOS>.
      append WA_ZLEST0008 to TI_ZLEST0008.
      modify ZLEST0008 from WA_ZLEST0008.


      P_VLRCONFIRMADO = 0.
      P_VLRORIGEM     = 0.
      P_VLRDIFERENCA  = 0.
      P_VLRREALIZADO  = 0.
      P_VLR_A_PAGAR   = 0.

    elseif ( LW_LANCTO-CHVID eq '19' ).
      P_VLR_ACREC_DESC = P_VLR_ACREC_DESC + LW_LANCTO-VLRCONFIRMADO.
    elseif  ( LW_LANCTO-CHVID eq '20' ).
      P_VLR_ACREC_DESC = P_VLR_ACREC_DESC - LW_LANCTO-VLRCONFIRMADO.
    else.
      P_VLRCONFIRMADO  = P_VLRCONFIRMADO  + LW_LANCTO-VLRCONFIRMADO.
      P_VLRORIGEM      = P_VLRORIGEM      + LW_LANCTO-VLRORIGEM.
      P_VLRDIFERENCA   = P_VLRDIFERENCA   + LW_LANCTO-VLRDIFERENCA.
      P_VLRREALIZADO   = P_VLRREALIZADO   + LW_LANCTO-VLRPROGRAMADO.
    endif.

  endloop.

  P_VLR_A_PAGAR = P_VLRREALIZADO + P_VLR_ACREC_DESC.

endform.                    " YF_OBTEM_VLRITENS_ACUMULADOS

*&---------------------------------------------------------------------*
*&      Form  YF_CHECK_LOG_LES0008
*&---------------------------------------------------------------------*
form YF_CHECK_LOG_LES0008  using P_LOTE
                        changing P_ICON_LOG.

  clear P_ICON_LOG.

  read table TI_ZLEST0008 with key LOTE = P_LOTE binary search.
  check SY-SUBRC is  initial.

  P_ICON_LOG = ICON_MESSAGE_WARNING_SMALL.

endform.                    " YF_CHECK_LOG_LES0008

*&---------------------------------------------------------------------*
*&      Form  YF_CONVERTE_VLR_UNID_PESO
*&---------------------------------------------------------------------*
form YF_CONVERTE_VLR_UNID_PESO using P_UNID_ORIG type GEWEI
                                     P_UNID_DEST type GEWEI
                            changing P_VALOR     type P.

  data: LV_FACTOR type F,
        LV_VALUE  type F,
        LV_VALOR  type ZQUANT17_7.

  check: P_UNID_ORIG <> P_UNID_DEST and P_VALOR <> 0.

* Trabalha com casas decimais maiores
  LV_VALOR = P_VALOR.

* Obtem fator de conversão
  call function 'MC_UNIT_CONVERSION'
    exporting
      NACH_MEINS           = P_UNID_DEST
      VON_MEINS            = P_UNID_ORIG
    importing
      UMREF                = LV_FACTOR
    exceptions
      CONVERSION_NOT_FOUND = 1
      MATERIAL_NOT_FOUND   = 2
      NACH_MEINS_MISSING   = 3
      OVERFLOW             = 4
      VON_MEINS_MISSING    = 5
      others               = 6.

  check: SY-SUBRC is initial.

  LV_VALUE = LV_VALOR * LV_FACTOR.
  clear LV_VALOR.

* Aplica arredondamento para sete casas decimais
  call function 'ROUND'
    exporting
      DECIMALS      = 7
      INPUT         = LV_VALUE
      SIGN          = 'X'
    importing
      OUTPUT        = LV_VALOR
    exceptions
      INPUT_INVALID = 1
      OVERFLOW      = 2
      TYPE_INVALID  = 3
      others        = 4.

* Devolve o valor para casas decimais informada
  P_VALOR = LV_VALOR.

endform.                    " YF_CONVERTE_VLR_UNID_PESO

*&---------------------------------------------------------------------*
*&      Form  YF_CLASSIFICA_RESULTADO
*&---------------------------------------------------------------------*
form YF_CLASSIFICA_RESULTADO  tables
                         PT_LOTES    type  ZLES_COCKPIT_LOTE_T
                         PT_LANCTOS  type  ZLES_COCKPIT_LANCTO_T
                         PT_DELTAS   type  ZLES_COCKPIT_DELTA_T
                         PT_CONFER   type  ZLES_COCKPIT_CONFER_T
                         PT_ACRDECR  type  ZLES_COCKPIT_ACRESCDECRES_T.

  check: not PT_LOTES[] is initial.

* ------------------------------------------------------------------
* NOTA: Antes de alterar a classificação verificar o seu uso
*       na chamada desta função
* ------------------------------------------------------------------

  sort: PT_LOTES   by CODTRP CODPOSTO LOTE CTAFRETE CONHEC CHVID
                      STATUS DATALOTE,
        PT_LANCTOS by CODTRP CODPOSTO LOTE CTAFRETE CONHEC
                      CHVID DATALOTE,
        PT_DELTAS  by CODTRP CODPOSTO LOTE CTAFRETE CONHEC
                      CHVID DATALOTE,
        PT_CONFER  by CODTRP CODPOSTO LOTE CHVID
                      TIPTRANSP CTLGLANCTO,
        PT_ACRDECR by CODTRP CODPOSTO LOTE CHVID
                      BUKRS GJAHR DOCSAP.

endform.                    " YF_CLASSIFICA_RESULTADO


*&---------------------------------------------------------------------*
*&      Form  ADD
*&---------------------------------------------------------------------*
*       Add código HTML - Truncar
*----------------------------------------------------------------------*
form ADD  tables   P_HTML  structure W3HTML
          using    P_TEXTO type STRING.
  call function 'ZHTML_ADD'
    exporting
      I_TEXTO = P_TEXTO
    tables
      IT_HTML = P_HTML.
endform.                    " ADD

*&---------------------------------------------------------------------*
*&      Form  YF_CALC_DIAS_VENCIMENTO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form YF_CALC_DIAS_VENCIMENTO using LC_DIAS type C.

  data: WA_SETLEAF type SETLEAF,
        IT_SETLEAF like table of WA_SETLEAF initial size 0 with header line.

  "Opter Área de contabilidade de custos
  select * into table IT_SETLEAF
    from SETLEAF
   where SETNAME eq 'ZDIASVENCIMENTO_POSTOS'.

  if not SY-SUBRC is initial.
    message E063.
  endif.

  loop at IT_SETLEAF into WA_SETLEAF.
    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        INPUT  = WA_SETLEAF-VALFROM
      importing
        OUTPUT = LC_DIAS.
  endloop.

endform.                    " YF_CALC_DIAS_VENCIMENTO

*&---------------------------------------------------------------------*
*&      Form  YF_DOCUMENTOS_GERADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_T_LANCTOS  text
*      -->P_T_CONFER  text
*----------------------------------------------------------------------*
form YF_DOCUMENTOS_GERADOS  tables   PT_LANCTOS  type  ZLES_COCKPIT_LANCTO_T
                                     PT_CONFER   type  ZLES_COCKPIT_CONFER_T.

  data: WA_LANCTOS type ZLES_COCKPIT_LANCTO,
        WA_CONFER  type ZLES_COCKPIT_CONFER,
        VG_TABIX   type SY-TABIX.

  sort: PT_LANCTOS by CODTRP CODPOSTO LOTE CTAFRETE CONHEC
                      CHVID,
        PT_CONFER  by CODTRP CODPOSTO LOTE CHVID
                      CTLGLANCTO CONHECIMENTO.

  loop at PT_LANCTOS into WA_LANCTOS.

    VG_TABIX = SY-TABIX.

    read table PT_CONFER into WA_CONFER with key
        CODTRP       = WA_LANCTOS-CODTRP
        CODPOSTO     = WA_LANCTOS-CODPOSTO
        LOTE         = WA_LANCTOS-LOTE
        CHVID        = WA_LANCTOS-CHVID
        CTLGLANCTO   = 'VC'
        CONHECIMENTO = WA_LANCTOS-CONHEC.

    if SY-SUBRC is initial.
      WA_LANCTOS-DOCSAP1 = WA_CONFER-DOCSAP.
    endif.

    read table PT_CONFER into WA_CONFER with key
        CODTRP       = WA_LANCTOS-CODTRP
        CODPOSTO     = WA_LANCTOS-CODPOSTO
        LOTE         = WA_LANCTOS-LOTE
        CHVID        = WA_LANCTOS-CHVID
        CTLGLANCTO   = 'S'
        CONHECIMENTO = WA_LANCTOS-CONHEC.

    if SY-SUBRC is initial.
      WA_LANCTOS-DOCSAP2 = WA_CONFER-DOCSAP.
    else.
      read table PT_CONFER into WA_CONFER with key
          CODTRP       = WA_LANCTOS-CODTRP
          CODPOSTO     = WA_LANCTOS-CODPOSTO
          LOTE         = WA_LANCTOS-LOTE
          CHVID        = WA_LANCTOS-CHVID
          CTLGLANCTO   = 'Q'
          CONHECIMENTO = WA_LANCTOS-CONHEC.
      if SY-SUBRC is initial.
        WA_LANCTOS-DOCSAP2 = WA_CONFER-DOCSAP.
      endif.
    endif.

    read table PT_CONFER into WA_CONFER with key
        CODTRP       = WA_LANCTOS-CODTRP
        CODPOSTO     = WA_LANCTOS-CODPOSTO
        LOTE         = WA_LANCTOS-LOTE
        CHVID        = WA_LANCTOS-CHVID
        CTLGLANCTO   = 'P'
        CONHECIMENTO = WA_LANCTOS-CONHEC.

    if SY-SUBRC is initial.
      WA_LANCTOS-DOCSAP3 = WA_CONFER-DOCSAP.
    endif.

    modify PT_LANCTOS index VG_TABIX from WA_LANCTOS transporting DOCSAP1 DOCSAP2 DOCSAP3.

  endloop.

endform.                    " YF_DOCUMENTOS_GERADOS

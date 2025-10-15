*&--------------------------------------------------------------------&*
*&                        ROLLOUT - Consultoria                       &*
*&--------------------------------------------------------------------&*
*& Projeto..: AMaggi                                                  &*
*& Autor....: Eduardo Ruttkowski Tavares                              &*
*& Data.....: 12/06/2014                                              &*
*& Descrição: Atualização de dados de fluxo de caixa - previsto       &*
*& Transação:                                                         &*
*&--------------------------------------------------------------------&*
*& Projeto  :                                                         &*
*& Código Espec.Funcional/Técnica:                                    &*
*&--------------------------------------------------------------------&*
*&                    Histórico de Modificações                       &*
*& Autor           Request      Data         Descrição                &*
*& ABAP                         12.06.2014                            &*
*&--------------------------------------------------------------------&*

REPORT  zfir0052.

TABLES: bsik, faglflexa.

DATA: BEGIN OF tg_bsik OCCURS 0,
        bukrs LIKE bsik-bukrs,
        lifnr LIKE bsik-lifnr,
        umsks LIKE bsik-umsks,
        umskz LIKE bsik-umskz,
        augdt LIKE bsik-augdt,
        augbl LIKE bsik-augbl,
        zuonr LIKE bsik-zuonr,
        gjahr LIKE bsik-gjahr,
        belnr LIKE bsik-belnr,
        buzei LIKE bsik-buzei,
        budat LIKE bsik-budat,
        bldat LIKE bsik-bldat,
        waers LIKE bsik-waers,
        xblnr LIKE bsik-xblnr,
        blart LIKE bsik-blart,
        gsber LIKE bsik-gsber,
        ebeln LIKE bsik-ebeln,
        ebelp LIKE bsik-ebelp,
        bschl LIKE bsik-bschl,
        zfbdt LIKE bsik-zfbdt,
        zbd1t LIKE bsik-zbd1t,
        venci LIKE bsik-zfbdt,
      END OF tg_bsik.

DATA: BEGIN OF tg_flagl OCCURS 0,
        ryear   LIKE faglflexa-ryear ,
        docnr   LIKE faglflexa-docnr ,
        rldnr   LIKE faglflexa-rldnr ,
        rbukrs  LIKE faglflexa-rbukrs ,
        docln   LIKE faglflexa-docln,
        racct   LIKE faglflexa-racct ,
        hsl     LIKE faglflexa-hsl ,
        ksl     LIKE faglflexa-ksl ,
        drcrk   LIKE faglflexa-drcrk ,
        buzei   LIKE faglflexa-buzei ,
        bschl   LIKE faglflexa-bschl ,
        usnam   LIKE faglflexa-usnam,
      END OF tg_flagl,

      BEGIN OF tg_0078 OCCURS 0,
        saknr   LIKE zfit0078-saknr,
        cod_flx LIKE zfit0078-cod_flx,
      END OF tg_0078.

DATA: tg_0079 LIKE zfit0079 OCCURS 0 WITH HEADER LINE.

*----------------------------------------------------------------------*
* TELA DE SELEÇÃO
*----------------------------------------------------------------------*
SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS: s_bukrs FOR bsik-bukrs OBLIGATORY,
                s_lifnr FOR bsik-lifnr OBLIGATORY,
                s_budat FOR bsik-budat OBLIGATORY,
                s_zfbdt FOR bsik-zfbdt OBLIGATORY.
SELECTION-SCREEN: END OF BLOCK b1.

*----------------------------------------------------------------------*
* START-OF-SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM seleciona_dados.

  PERFORM processa_dados.

  PERFORM grava_dados.
*&---------------------------------------------------------------------*
*&      Form  SELECIONA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM seleciona_dados .

*** Selecionar os dados da BSIK sem considerar o campo S_ZFBDT
* após a seleção, somar os campos ZFBDT + ZBD1T e validar se a soma
* está dentro de S_ZFBDT

* Selecionar dados BSIK com:
* BUKRS
* LIFNR
* BUDAT

  SELECT bukrs lifnr umsks umskz augdt augbl
         zuonr gjahr belnr buzei budat bldat
         waers xblnr blart gsber ebeln ebelp
         bschl zfbdt zbd1t
    FROM bsik INTO TABLE tg_bsik
    WHERE bukrs IN s_bukrs AND
          lifnr IN s_lifnr AND
          budat IN s_budat.


  IF sy-subrc <> 0.
    MESSAGE s000(z01) DISPLAY LIKE 'E' WITH 'Não foram encontrados registros na tabela BSIK.'.
  ELSE.
* Fazer um loop no retorno somando ZFBDT + ZBD1T.
* se a soma não estiver em S_ZFBDT, descartar o registro

    LOOP AT tg_bsik.
      tg_bsik-venci = tg_bsik-zfbdt + tg_bsik-zbd1t.
      IF tg_bsik-venci IN s_zfbdt.
        MODIFY tg_bsik.
      ELSE.
        DELETE tg_bsik.
      ENDIF.
    ENDLOOP.

  ENDIF.

  IF tg_bsik[] IS INITIAL.

  ELSE.
    SELECT ryear docnr rldnr rbukrs docln
           racct hsl ksl drcrk buzei bschl usnam
      FROM faglflexa INTO TABLE tg_flagl
      FOR ALL ENTRIES IN tg_bsik
      WHERE ryear  = tg_bsik-gjahr AND
            docnr  = tg_bsik-belnr AND
            rldnr  = '0L'          AND
            rbukrs = tg_bsik-bukrs AND
            bschl  = tg_bsik-bschl.

    IF sy-subrc IS NOT INITIAL.
      MESSAGE s000(z01) DISPLAY LIKE 'E' WITH 'Não foram encontrados registros na tabela FAGLFLEXA.'.
    ELSE.
      SELECT saknr cod_flx
        FROM zfit0078 INTO TABLE tg_0078
        FOR ALL ENTRIES IN tg_flagl
        WHERE saknr = tg_flagl-racct.

      IF sy-subrc IS NOT INITIAL.
        MESSAGE s000(z01) DISPLAY LIKE 'E' WITH 'Não foram encontrados registros na tabela ZFIT0078.'.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    " SELECIONA_DADOS
*&---------------------------------------------------------------------*
*&      Form  PROCESSA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM processa_dados .

  SORT: tg_flagl BY ryear docnr rldnr rbukrs bschl,
        tg_0078  BY saknr.

  DATA: vl_data TYPE sy-datum,
        vl_hora TYPE sy-uzeit.

  vl_hora = sy-uzeit.
  vl_data = sy-datum.

  CLEAR tg_0079. REFRESH tg_0079.

* agrupar os dados selecionados em uma tabela
  LOOP AT tg_bsik.
    READ TABLE tg_flagl
      WITH KEY ryear  = tg_bsik-gjahr
               docnr  = tg_bsik-belnr
               rldnr  = '0L'
               rbukrs = tg_bsik-bukrs
               bschl  = tg_bsik-bschl BINARY SEARCH.

    IF sy-subrc IS INITIAL.
      READ TABLE tg_0078
        WITH KEY saknr = tg_flagl-racct BINARY SEARCH.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

    ELSE.
      CONTINUE.
    ENDIF.

    MOVE: tg_bsik-bukrs     TO tg_0079-bukrs,
          tg_bsik-lifnr     TO tg_0079-lifnr,
          tg_bsik-belnr     TO tg_0079-belnr,
          tg_bsik-gjahr     TO tg_0079-gjahr,
          tg_flagl-buzei    TO tg_0079-buzei,
          tg_bsik-budat     TO tg_0079-budat,
          tg_bsik-venci     TO tg_0079-zfbdt,
          tg_flagl-racct    TO tg_0079-hkont,
          tg_0078-cod_flx   TO tg_0079-cod_flx,
          tg_bsik-bldat     TO tg_0079-bldat,
          tg_bsik-waers     TO tg_0079-waers,
          tg_bsik-xblnr     TO tg_0079-xblnr,
          tg_bsik-blart     TO tg_0079-blart,
          tg_bsik-gsber     TO tg_0079-gsber,
          tg_bsik-ebeln     TO tg_0079-ebeln,
          tg_bsik-ebelp     TO tg_0079-ebelp,
          tg_flagl-bschl    TO tg_0079-bschl,
          tg_flagl-drcrk    TO tg_0079-shkzg,
          tg_flagl-hsl      TO tg_0079-dmbtr,
          tg_flagl-ksl      TO tg_0079-dmbe2,
          tg_flagl-usnam    TO tg_0079-usnam,
          vl_data           TO tg_0079-dt_atual,
          vl_hora           TO tg_0079-hr_atual.

    APPEND: tg_0079.
    CLEAR: tg_0079.

  ENDLOOP.

ENDFORM.                    " PROCESSA_DADOS
*&---------------------------------------------------------------------*
*&      Form  GRAVA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM grava_dados .
  DATA: wl_lines TYPE i.

  IF tg_0079[] IS NOT INITIAL.
    MODIFY zfit0079 FROM TABLE tg_0079.

    DESCRIBE TABLE tg_0079 LINES wl_lines.
    MESSAGE s000(z01) DISPLAY LIKE 'S' WITH 'Foram gravados' wl_lines 'registros na tabela ZFIT0079.'.

    COMMIT WORK.

  ELSE.
    MESSAGE s000(z01) DISPLAY LIKE 'E' WITH 'Não foi possível gravar registros na tabela ZFIT0079.'.
  ENDIF.

ENDFORM.                    " GRAVA_DADOS

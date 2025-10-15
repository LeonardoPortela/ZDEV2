*&---------------------------------------------------------------------*
*& Report  ZMMR166
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zmmr166.

TABLES: zmmt0135,
        zmmt0136,
        zmmt0137,
        zmmt0138,
        zmmt0139,
        zmmt0140,
        zmmt0141,
        zmmt0142,
        zmmt0143,
        zmmt0144.

*---------------------------------------------------------------------*
* TAG                                                                 *
*---------------------------------------------------------------------*
DEFINE add_tag.
  CONCATENATE gva_centro '''' &1 '''' &2  INTO gva_centro.
END-OF-DEFINITION.

*&---------------------------------------------------------------------*
*& TABELAS / WORK AREAS
*&---------------------------------------------------------------------*
DATA: it_mseg      TYPE TABLE OF ty_mseg,
      wa_mseg      TYPE ty_mseg,
      git_zmmt0135 TYPE TABLE OF zmmt0135,
      gwa_zmmt0135 TYPE zmmt0135,
      git_zmmt0136 TYPE TABLE OF zmmt0136,
      gwa_zmmt0136 TYPE zmmt0136,
      git_zmmt0137 TYPE TABLE OF zmmt0137,
      gwa_zmmt0137 TYPE zmmt0137,
      git_zmmt0138 TYPE TABLE OF zmmt0138,
      gwa_zmmt0138 TYPE zmmt0138,
      git_zmmt0139 TYPE TABLE OF zmmt0139,
      gwa_zmmt0139 TYPE zmmt0139,
      git_zmmt0140 TYPE TABLE OF zmmt0140,
      gwa_zmmt0140 TYPE zmmt0140,
      git_zmmt0141 TYPE TABLE OF zmmt0141,
      gwa_zmmt0141 TYPE zmmt0141,
      git_zmmt0142 TYPE TABLE OF zmmt0142,
      gwa_zmmt0142 TYPE zmmt0142,
      git_zmmt0143 TYPE TABLE OF zmmt0143,
      gwa_zmmt0143 TYPE zmmt0143,
      git_zmmt0144 TYPE TABLE OF zmmt0144,
      gwa_zmmt0144 TYPE zmmt0144.

*&---------------------------------------------------------------------*
*& VERIFICAR JOB EM EXECUCAO
*&---------------------------------------------------------------------*
DATA: lva_job     TYPE i.

SELECT SINGLE COUNT( * ) INTO lva_job
  FROM tbtco
 WHERE jobname EQ 'MAGGI_ZMMR166'
   AND status EQ 'R'.

CHECK ( lva_job   EQ 1 ).

*&---------------------------------------------------------------------*
*& PREPARAR SALDO INICIAL
*&---------------------------------------------------------------------*

DATA: git_rsparams TYPE TABLE OF rsparams,
      gwa_rsparams TYPE rsparams,
      git_centro   TYPE TABLE OF setleaf.

DATA: lva_data1     TYPE sy-datum,
      lva_data2     TYPE sy-datum,
      lva_data3     TYPE sy-datum,
      lva_mesano(6) TYPE c.

*&---------------------------------------------------------------------*
*& PREPARA SET COM VARIAVEIS DE CENTRO (WERKS)
*&---------------------------------------------------------------------*

SELECT *
  FROM setleaf INTO TABLE git_centro
 WHERE setname = 'ZMMR166_CENTROS'.

LOOP AT git_centro INTO DATA(gwa_centro).
  gwa_rsparams-selname = 'SL_WERKS'.
  gwa_rsparams-kind    = 'S'.
  gwa_rsparams-sign    = 'I'.
  gwa_rsparams-option  = 'EQ'.
  gwa_rsparams-low     = gwa_centro-valfrom.
  CLEAR gwa_rsparams-high.
  APPEND gwa_rsparams TO git_rsparams.
  CLEAR: gwa_rsparams.
ENDLOOP.

DATA: r_werks TYPE RANGE OF werks_d.
r_werks = VALUE rsis_t_range( FOR l IN git_centro ( low = |{ l-valfrom }| option = 'EQ' sign = 'I' ) ).


DATA: gva_centro TYPE string,
      lva_lines  TYPE i.

DESCRIBE TABLE git_centro LINES lva_lines.

CONCATENATE gva_centro '(' INTO gva_centro.
DO lva_lines TIMES.
  READ TABLE git_centro INTO gwa_centro INDEX sy-index.
  IF sy-index <>  lva_lines.
    add_tag gwa_centro-valfrom   ','.
  ELSE.
    add_tag gwa_centro-valfrom   ''.
  ENDIF.
ENDDO.
CONCATENATE gva_centro ')' INTO gva_centro.

*&---------------------------------------------------------------------*
*& PREPARA PERIODOS PARA EXECUCAO ZRMCBS039
*&---------------------------------------------------------------------*

lva_data1 = sy-datum - 731.
CONCATENATE lva_data1+0(4) lva_data1+4(2) '01' INTO lva_data2.

CALL FUNCTION 'HRAR_SUBTRACT_MONTH_TO_DATE'
  EXPORTING
    date                 = lva_data2
  IMPORTING
    date_minus_one_month = lva_data3.

CONCATENATE lva_data3+0(4) lva_data3+4(2) INTO lva_mesano.
CLEAR: gwa_rsparams.
gwa_rsparams-selname = 'SL_SPMON'.
gwa_rsparams-kind    = 'S'.
gwa_rsparams-sign    = 'I'.
gwa_rsparams-option  = 'EQ'.
gwa_rsparams-low     = lva_mesano.
APPEND gwa_rsparams TO git_rsparams.


SUBMIT zrmcbs039  WITH SELECTION-TABLE git_rsparams
                  AND RETURN.

*&---------------------------------------------------------------------*
*& LIMPA TODAS AS TABELAS
*&---------------------------------------------------------------------*

DELETE FROM zmmt0135 CLIENT SPECIFIED WHERE mandt EQ sy-mandt.
DELETE FROM zmmt0136 CLIENT SPECIFIED WHERE mandt EQ sy-mandt.
DELETE FROM zmmt0137 CLIENT SPECIFIED WHERE mandt EQ sy-mandt.
DELETE FROM zmmt0138 CLIENT SPECIFIED WHERE mandt EQ sy-mandt.
DELETE FROM zmmt0139 CLIENT SPECIFIED WHERE mandt EQ sy-mandt.
DELETE FROM zmmt0140 CLIENT SPECIFIED WHERE mandt EQ sy-mandt.
DELETE FROM zmmt0141 CLIENT SPECIFIED WHERE mandt EQ sy-mandt.
DELETE FROM zmmt0142 CLIENT SPECIFIED WHERE mandt EQ sy-mandt.
DELETE FROM zmmt0143 CLIENT SPECIFIED WHERE mandt EQ sy-mandt.
DELETE FROM zmmt0144 CLIENT SPECIFIED WHERE mandt EQ sy-mandt.

COMMIT WORK.

*&---------------------------------------------------------------------*
*& Gerar Fato Movimentações
*&---------------------------------------------------------------------*
TRY.
    EXEC SQL.
      OPEN SQL_FATO_MOVIMENTACOES for
          select rs.dt_mov AS DATA_MOV,
           rs.werks AS ID_CENTRO,
           rs.matnr AS ID_MATERIAL,
           mara.mtart AS ID_TIPO,
           mara.matkl AS ID_GRUPO,
           rs.lgort AS ID_DEPOSITO,
           rs.Bwart AS ID_TP_MOV,
           QEntrada AS QTD_ENTRADA,
           Qsaida AS QTD_SAIDA,
           ltrim(TO_CHAR(preco, 'FM9999999.90')) as PRECO
      from (select rs.dt_mov,
                   rs.werks,
                   rs.matnr,
                   rs.lgort,
                   rs.Bwart,
                   sum(QEntrada) as QEntrada,
                   sum(Qsaida) as Qsaida,
                   sum(preco) / count(*) as preco
              from (select mseg.BUDAT_MKPF as dt_mov,
                           mseg.werks,
                           mseg.matnr,
                           mseg.lgort,
                           mseg.Bwart,
                           case
                             when mseg.SHKZG = 'S' then
                              mseg.menge
                             else
                              0
                           end as QEntrada,
                           case
                             when mseg.SHKZG = 'H' then
                              mseg.menge
                             else
                              0
                           end as Qsaida,
                           case
                             when mseg.DMBTR <> 0 then
                              mseg.DMBTR / mseg.menge
                             when (select ekbe.DMBTR / ekbe.menge
                                     from SAPHANADB.ekbe ekbe
                                    where ekbe.ebeln = mseg.ebeln
                                      and ekbe.ebelp = mseg.ebelp
                                      and ekbe.belnr = mseg.mblnr) <> 0 then
                              (select ekbe.DMBTR / ekbe.menge
                                 from SAPHANADB.ekbe ekbe
                                where ekbe.ebeln = mseg.ebeln
                                  and ekbe.ebelp = mseg.ebelp
                                  and ekbe.belnr = mseg.mblnr)
                             else
                              mseg.EXBWR / mseg.menge
                           end as preco
                      from SAPHANADB.mseg mseg
                     where mseg.mandt = '300'
                       and mseg.mandt = mseg.mandt
                       and mseg.werks in ('1002' , '1009', '9121' )
                       and mseg.BUDAT_MKPF >= ( substr( CURRENT_DATE ,1,6) || '01' )
                       and mseg.lgort <> ' '


                    ) rs

             group by rs.dt_mov, rs.werks, rs.matnr, rs.lgort, rs.Bwart
            having sum(Qentrada) > 0 or sum(Qsaida) > 0 or BWART = '000'
             order by rs.werks, rs.matnr, rs.lgort, dt_mov, rs.Bwart) rs,
           SAPHANADB.mara mara
     where mara.mandt = '300'
       and rs.matnr = mara.matnr
     order by rs.werks, rs.matnr, rs.lgort, rs.dt_mov, rs.Bwart

    ENDEXEC.

  CATCH cx_sy_native_sql_error INTO DATA(exc_ref).
    DATA(error_text) = exc_ref->get_text( ).
    MESSAGE error_text TYPE 'E'.
ENDTRY.

DO.
  EXEC SQL.
    FETCH NEXT SQL_FATO_MOVIMENTACOES INTO
      :GWA_zmmt0135-DATA_MOV,
      :GWA_zmmt0135-ID_CENTRO,
      :GWA_zmmt0135-ID_MATERIAL,
      :GWA_zmmt0135-ID_TIPO,
      :GWA_zmmt0135-ID_GRUPO,
      :GWA_zmmt0135-ID_DEPOSITO,
      :GWA_zmmt0135-ID_TP_MOV,
      :GWA_zmmt0135-QTD_ENTRADA,
      :GWA_zmmt0135-QTD_SAIDA,
      :GWA_zmmt0135-PRECO
  ENDEXEC.
  IF sy-subrc <> 0.
    EXIT.
  ELSE.
    gwa_zmmt0135-mandt = sy-mandt.
*    MODIFY zmmt0135 FROM gwa_zmmt0135.
    APPEND gwa_zmmt0135 TO git_zmmt0135.
    CLEAR gwa_zmmt0135.
  ENDIF.
ENDDO.

EXEC SQL.
  CLOSE SQL_FATO_MOVIMENTACOES
ENDEXEC.
COMMIT WORK.

IF git_zmmt0135 IS NOT INITIAL.
  MODIFY zmmt0135 FROM TABLE git_zmmt0135.
  COMMIT WORK.

*&---------------------------------------------------------------------*
*& Gerar Dimensões das Movimentações
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& DIM MATERIAL
  SELECT matnr, mtart, matkl FROM mara
  INTO TABLE @DATA(git_mara)
  FOR ALL ENTRIES IN @git_zmmt0135
  WHERE matnr  = @git_zmmt0135-id_material.

  SELECT matnr, maktx  FROM makt
    INTO TABLE @DATA(git_makt)
    FOR ALL ENTRIES IN @git_mara
  WHERE matnr  = @git_mara-matnr
  AND   spras = 'P'.

  SELECT mtart, mtbez FROM t134t
    INTO TABLE @DATA(git_t134t)
    FOR ALL ENTRIES IN @git_mara
  WHERE mtart  = @git_mara-mtart
  AND   spras = 'P'.

  SELECT matkl, wgbez FROM t023t
    INTO TABLE @DATA(git_t023t)
    FOR ALL ENTRIES IN @git_mara
  WHERE matkl  = @git_mara-matkl
  AND   spras = 'P'.

  LOOP AT git_mara INTO DATA(gwa_mara).

    READ TABLE git_makt  INTO DATA(gwa_makt)   WITH KEY matnr  = gwa_mara-matnr.
    READ TABLE git_t134t INTO DATA(gwa_t134t)  WITH KEY mtart  = gwa_mara-mtart.
    READ TABLE git_t023t INTO DATA(gwa_t023t)  WITH KEY matkl  = gwa_mara-matkl.
    IF sy-subrc = 0.
      gwa_zmmt0136-mandt = sy-mandt.
      gwa_zmmt0136-id_material     =  gwa_mara-matnr.
      gwa_zmmt0136-descr_material  =  gwa_makt-maktx.
      gwa_zmmt0136-id_tipo         =  gwa_mara-mtart.
      gwa_zmmt0136-descr_tipo      =  gwa_t134t-mtbez.
      gwa_zmmt0136-id_grupo        =  gwa_mara-matkl.
      gwa_zmmt0136-descr_grup      =  gwa_t023t-wgbez.

      MODIFY zmmt0136 FROM gwa_zmmt0136.
      CLEAR gwa_zmmt0136.
    ENDIF.
  ENDLOOP.
  COMMIT WORK.

*&---------------------------------------------------------------------*
*& DIM MATERIAL+CENTRO+DEPOSITO

  SELECT matnr, werks, bstmi , plifz FROM marc
    INTO TABLE @DATA(git_marc)
    FOR ALL ENTRIES IN @git_zmmt0135
  WHERE matnr  = @git_zmmt0135-id_material
    AND werks  = @git_zmmt0135-id_centro.

  SELECT matnr, werks, lgort , ersda FROM mard
    INTO TABLE @DATA(git_mard)
    FOR ALL ENTRIES IN @git_zmmt0135
  WHERE matnr  =  @git_zmmt0135-id_material
    AND werks  =  @git_zmmt0135-id_centro
    AND lgort  =  @git_zmmt0135-id_deposito.


  LOOP AT git_zmmt0135 INTO DATA(gwa_zmmt0135_aux).

    READ TABLE git_marc INTO DATA(gwa_marc) WITH KEY matnr  = gwa_zmmt0135_aux-id_material
                                                     werks  = gwa_zmmt0135_aux-id_centro.

    READ TABLE git_mard INTO DATA(gwa_mard) WITH KEY matnr  =  gwa_zmmt0135_aux-id_material
                                                     werks  =  gwa_zmmt0135_aux-id_centro
                                                     lgort  =  gwa_zmmt0135_aux-id_deposito.

    IF sy-subrc = 0.
      gwa_zmmt0137-mandt               = sy-mandt.
      gwa_zmmt0137-id_centro           = gwa_marc-werks.
      gwa_zmmt0137-id_material         = gwa_marc-matnr.
      gwa_zmmt0137-id_deposito         = gwa_mard-lgort.
      gwa_zmmt0137-lote_minimo_compra  = gwa_marc-bstmi.
      gwa_zmmt0137-data_expansao       = gwa_mard-ersda.
      gwa_zmmt0137-leadtime            = gwa_marc-plifz.

      MODIFY zmmt0137 FROM gwa_zmmt0137.
      CLEAR:gwa_zmmt0137.
    ENDIF.
  ENDLOOP.
  COMMIT WORK.

*&---------------------------------------------------------------------*
*& DIM_DEPOSITO
  SELECT werks, lgort, lgobe  FROM t001l
    INTO TABLE @DATA(git_t001l)
    FOR ALL ENTRIES IN @git_zmmt0135
  WHERE werks  =  @git_zmmt0135-id_centro
    AND lgort  =  @git_zmmt0135-id_deposito.

  LOOP AT git_t001l INTO DATA(gwa_t001l).
    gwa_zmmt0138-mandt          = sy-mandt.
    gwa_zmmt0138-id_centro      = gwa_t001l-werks.
    gwa_zmmt0138-id_deposito    = gwa_t001l-lgort.
    gwa_zmmt0138-descr_deposito = gwa_t001l-lgobe.

    MODIFY zmmt0138 FROM gwa_zmmt0138.
    CLEAR:gwa_zmmt0138.
  ENDLOOP.
  COMMIT WORK.

ENDIF.
*&---------------------------------------------------------------------*
*& DIM_TIPO_MOVIMENTO
TRY.
    EXEC SQL.
      OPEN SQL_TIPO_MOVIMENTO for
        with SELECAO AS
        ( select  distinct BWART, KZBEW, SOBKZ, KZZUG, KZVBR
            from SAPHANADB.mseg  mseg
            where mseg.mandt = '300'
            and mseg.werks in ('1002' , '1009', '9121' )
            and mseg.lgort <> ' '
            and mseg.budat_mkpf >= substr( current_date ,1,6) || '01'   )


        SELECT DISTINCT T.BWART AS ID_TP_MOV,
                        T.BTEXT AS DESC_TP_MOV
        FROM selecao,
            SAPHANADB.T156T t
        where t.bwart = selecao.bwart
        and   t.spras = 'P'
        and   t.KZBEW = selecao.KZBEW
        and   t.SOBKZ = selecao.SOBKZ
        and   t.KZZUG = selecao.KZZUG
        and   t.KZVBR = selecao.KZVBR
        order by T.BWART

    ENDEXEC.
  CATCH cx_sy_native_sql_error INTO DATA(exc_ref_tpmov).
    DATA(error_text_tpmov) = exc_ref_tpmov->get_text( ).
    MESSAGE error_text_tpmov TYPE 'E'.
ENDTRY.

DO.
  EXEC SQL.
    FETCH NEXT SQL_TIPO_MOVIMENTO INTO
    :GWA_ZMMT0139-ID_TP_MOV,
    :GWA_ZMMT0139-DESC_TP_MOV
  ENDEXEC.
  IF sy-subrc <> 0.
    EXIT.
  ELSE.
    gwa_zmmt0139-mandt = sy-mandt.
    MODIFY zmmt0139 FROM gwa_zmmt0139.
    APPEND gwa_zmmt0139 TO git_zmmt0139.
    CLEAR: gwa_zmmt0139.
  ENDIF.
ENDDO.
COMMIT WORK.

EXEC SQL.
  CLOSE SQL_TIPO_MOVIMENTO
ENDEXEC.

*&---------------------------------------------------------------------*
*& DIM_CENTRO

SELECT  *
  FROM t001w
INTO TABLE @DATA(git_t001w)
  WHERE werks IN @r_werks
  ORDER BY werks.

LOOP AT git_t001w INTO DATA(gwa_t001w).
  gwa_zmmt0140-mandt        = sy-mandt.
  gwa_zmmt0140-id_centro    = gwa_t001w-werks.
  gwa_zmmt0140-descr_centro = gwa_t001w-name1.
  MODIFY zmmt0140 FROM gwa_zmmt0140.
  CLEAR: gwa_zmmt0140.
ENDLOOP.
COMMIT WORK.


*&---------------------------------------------------------------------*
*& DIM_SALDO
SELECT  *
  FROM s039
INTO TABLE @DATA(git_s039)
  WHERE werks IN @r_werks
    AND vrsio   = '000'
    AND spmon  = @lva_mesano
    AND lgort <> ' '
ORDER BY spmon,
         werks,
         matnr.

LOOP AT git_s039 INTO DATA(gwa_s039).
  gwa_zmmt0141-mandt        =   sy-mandt.
  gwa_zmmt0141-mesano       =   gwa_s039-spmon.
  gwa_zmmt0141-id_centro    =   gwa_s039-werks.
  gwa_zmmt0141-id_material  =   gwa_s039-matnr.
  gwa_zmmt0141-id_deposito  =   gwa_s039-lgort.
  gwa_zmmt0141-qtd_saldo    =   gwa_s039-mbwbest.
  gwa_zmmt0141-vlr_saldo    =   gwa_s039-wbwbest.

  MODIFY zmmt0141 FROM gwa_zmmt0141.
  CLEAR: gwa_zmmt0141.
ENDLOOP.
COMMIT WORK.

*TRY.
*    EXEC SQL.
*      OPEN SQL_DIM_SALDO for
*       select s039.spmon as MES,
*              s039.werks as ID_CENTRO,
*              s039.matnr as ID_MATERIAL,
*              s039.lgort as ID_DEPOSITO,
*              s039.mbwbest as QTD_SALDO,
*              s039.WBWBEST as VLR_SALDO
*              from SAPHANADB.s039 s039
*              where s039.mandt = '300'
*              and s039.vrsio = '000'
*              and s039.spmon = substr(substr(TO_CHAR(sysdate - 731, 'YYYYMMDD') ,1,6) || '01'  - 2,1,6 )
*              and s039.werks in ('1002' , '1009', '9121' )
*              and s039.lgort <> ' '
*              order by s039.spmon,
*              s039.werks,
*              s039.matnr
*    ENDEXEC.
*  CATCH cx_sy_native_sql_error INTO DATA(exc_dim_saldo).
*    DATA(error_text_saldo) = exc_ref->get_text( ).
*    MESSAGE error_text TYPE 'E'.
*ENDTRY.
*
*DO.
*  EXEC SQL.
*    FETCH NEXT SQL_DIM_SALDO INTO
*    gwa_zmmt0141-mesano,
*    gwa_zmmt0141-id_centro,
*    gwa_zmmt0141-id_material,
*    gwa_zmmt0141-id_deposito,
*    gwa_zmmt0141-qtd_saldo,
*    gwa_zmmt0141-vlr_saldo
*  ENDEXEC.
*  IF sy-subrc <> 0.
*    EXIT.
*  ELSE.
*    gwa_zmmt0141-mandt  = sy-mandt.
*    MODIFY zmmt0141 FROM gwa_zmmt0141.
*    APPEND gwa_zmmt0141 TO git_zmmt0141.
*    CLEAR: gwa_zmmt0141.
*  ENDIF.
*ENDDO.
*COMMIT WORK.
*
*EXEC SQL.
*  CLOSE SQL_DIM_SALDO
*ENDEXEC.

*&---------------------------------------------------------------------*
*& Gerar Fato Requisições Compra
*&---------------------------------------------------------------------*
" Opção de teste qas: and rc.badat between '20190301 and '20190331'
TRY.
    EXEC SQL.
      OPEN SQL_FATO_REQ_COMPRA for
        select rc.werks      as ID_CENTRO,
           rc.BADAT      as DATA_REQUISICAO,
           rc.LFDAT      as DATA_REMESSA,
           rc.MATNR      as ID_MATERIAL,
           ma.mtart      AS ID_TIPO,
           ma.matkl      AS ID_GRUPO,
           rc.LGORT      as ID_DEPOSITO,
           sum(rc.menge) as QTDE_REMESSA
          from SAPHANADB.eban RC,
               SAPHANADB.mara ma
          where  rc.mandt = '300'
           and   rc.werks in ('1002' , '1009', '9121' )
           and   rc.knttp = ' '
           and   rc.lgort <> ' '
           and   rc.loekz = ' '
           and   rc.mandt = ma.mandt
           and   rc.matnr = ma.matnr

      and   not exists (select * from  SAPHANADB.ekpo IP
      where rc.mandt = IP.MANDT
      and   rc.BANFN = IP.BANFN
      and   rc.bnfpo = ip.bnfpo )
      group by  rc.werks,
            rc.badat,
            rc.lfdat,
            rc.matnr,
            ma.mtart,
            ma.matkl,
            rc.lgort

      order by
            rc.werks,
            rc.badat,
            rc.lfdat,
            rc.matnr,
            rc.lgort

    ENDEXEC.
  CATCH cx_sy_native_sql_error INTO DATA(exc_ref_compra).
    DATA(error_text_compra) = exc_ref_compra->get_text( ).
    MESSAGE error_text_compra TYPE 'E'.
ENDTRY.

DO.
  EXEC SQL.
    FETCH NEXT SQL_FATO_REQ_COMPRA INTO
    :GWA_ZMMT0142-ID_CENTRO,
    :GWA_ZMMT0142-DATA_REQUISICAO,
    :GWA_ZMMT0142-DATA_REMESSA,
    :GWA_ZMMT0142-ID_MATERIAL,
    :GWA_ZMMT0142-ID_TIPO,
    :GWA_ZMMT0142-ID_GRUPO,
    :GWA_ZMMT0142-ID_DEPOSITO,
    :GWA_ZMMT0142-QTDE_REMESSA
  ENDEXEC.
  IF sy-subrc <> 0.
    EXIT.
  ELSE.
    gwa_zmmt0142-mandt = sy-mandt.
    MODIFY zmmt0142 FROM gwa_zmmt0142.
    APPEND gwa_zmmt0142 TO git_zmmt0142.
    CLEAR: gwa_zmmt0142.
  ENDIF.
ENDDO.
COMMIT WORK.

EXEC SQL.
  CLOSE SQL_FATO_REQ_COMPRA
ENDEXEC.

*&---------------------------------------------------------------------*
*& Gerar Dimensões das Requisições de Compra
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& DIM FATO_PEDIDOS
*&---------------------------------------------------------------------*
" Opção de teste qas:  and   cb.aedat between '20190301' and '20190331'
TRY.
    EXEC SQL.
      OPEN SQL_FATO_PEDIDOS for
    select it.werks      as ID_CENTRO,
               re.EINDT      as DATA_REMESSA,
               cb.lifnr      as ID_FORNECEDOR,
               it.MATNR      as ID_MATERIAL,
               ma.mtart      AS ID_TIPO,
               ma.matkl      AS ID_GRUPO,
               it.LGORT      as ID_DEPOSITO,
               sum(re.menge) as QTDE_REMESSA,
               ltrim(TO_CHAR(sum(it.netwr) / sum(re.menge),'FM9999999.90'))  as PRECO
        from SAPHANADB.ekpo it,
             SAPHANADB.eket re,
             SAPHANADB.ekko cb,
             SAPHANADB.mara ma
        where  it.mandt = '300'
        and    it.werks in ('1002' , '1009', '9121' )
         and   it.knttp = ' '
         and   it.lgort <> ' '
         and   it.loekz = ' '
         and   it.menge > 0
         and   it.mandt = ma.mandt
         and   it.matnr = ma.matnr

         and   re.mandt = it.mandt
         and   re.ebeln = it.ebeln
         and   re.ebelp = it.ebelp

         and   cb.mandt = it.mandt
         and   cb.ebeln = it.ebeln
         and   cb.BSTYP = 'F'
      and   not exists (select * from  SAPHANADB.ekbe ek
      where ek.mandt = it.MANDT
      and   ek.ebeln = it.ebeln
      and   ek.ebelp = it.ebelp
      and   ek.bewtp = 'E')
      GROUP BY  it.werks,
      re.EINDT,
      cb.lifnr,
      it.matnr,
      ma.mtart,
      ma.matkl,
      it.lgort

    order by
      it.werks,
      re.EINDT,
      cb.lifnr,
      it.matnr,
      it.lgort
    ENDEXEC.
  CATCH cx_sy_native_sql_error INTO DATA(exc_ref_pedido).
    DATA(error_text_pedido) = exc_ref_pedido->get_text( ).
    MESSAGE error_text_pedido TYPE 'E'.
ENDTRY.

DO.
  EXEC SQL.
    FETCH NEXT SQL_FATO_PEDIDOS INTO
      :GWA_ZMMT0143-ID_CENTRO,
      :GWA_ZMMT0143-DATA_REMESSA,
      :GWA_ZMMT0143-ID_FORNECEDOR,
      :GWA_ZMMT0143-ID_MATERIAL,
      :GWA_ZMMT0143-ID_TIPO,
      :GWA_ZMMT0143-ID_GRUPO,
      :GWA_ZMMT0143-ID_DEPOSITO,
      :GWA_ZMMT0143-QTDE_REMESSA,
      :GWA_ZMMT0143-PRECO
  ENDEXEC.
  IF sy-subrc <> 0.
    EXIT.
  ELSE.
    gwa_zmmt0143-mandt = sy-mandt.
    MODIFY zmmt0143 FROM gwa_zmmt0143.
    APPEND gwa_zmmt0143 TO git_zmmt0143.
    CLEAR: gwa_zmmt0143.
  ENDIF.
ENDDO.
COMMIT WORK.

EXEC SQL.
  CLOSE SQL_FATO_PEDIDOS
ENDEXEC.

*&---------------------------------------------------------------------*
*& DIM FORNECEDOR
TRY.
    EXEC SQL.
      OPEN SQL_DIM_FORNECEDOR FOR
        select lf.LIFNR    as ID_FORNECEDOR,
               lf.NAME1    as DESCR_FORNECEDOR,
               lf.ORT01    as CIDADE,
               lf.REGIO   as UF
        from SAPHANADB.lfa1 lf
        where exists ( select * from SAPHANADB.ekpo it,
                                     SAPHANADB.ekko cb
                                where  cb.lifnr = lf.lifnr
                                 and   it.knttp = ' '
                                 and   it.lgort <> ' '
                                 and   it.loekz = ' '
                                 and   it.menge > 0
                                 and   it.werks in ('1002' , '1009', '9121' )
                                 and   cb.ebeln = it.ebeln
                                 and   cb.BSTYP = 'F'
                                 and   not exists (select * from  SAPHANADB.ekbe ek
                                                where ek.mandt = it.MANDT
                                                and   ek.ebeln = it.ebeln
                                                and   ek.ebelp = it.ebelp
                                                and   ek.bewtp = 'E')  )

    ENDEXEC.
  CATCH cx_sy_native_sql_error INTO DATA(exc_ref_fornec).
    DATA(error_text_fornec) = exc_ref_fornec->get_text( ).
    MESSAGE error_text_fornec TYPE 'E'.
ENDTRY.

DO.
  EXEC SQL.
    FETCH NEXT SQL_DIM_FORNECEDOR INTO
    :GWA_ZMMT0144-ID_FORNECEDOR,
    :GWA_ZMMT0144-DESCR_FORNECEDOR,
    :GWA_ZMMT0144-CIDADE,
    :GWA_ZMMT0144-UF
  ENDEXEC.
  IF sy-subrc <> 0.
    EXIT.
  ELSE.
    gwa_zmmt0144-mandt = sy-mandt.
    MODIFY zmmt0144 FROM gwa_zmmt0144.
    CLEAR:gwa_zmmt0144.
  ENDIF.
ENDDO.
COMMIT WORK.

EXEC SQL.
  CLOSE SQL_DIM_FORNECEDOR
ENDEXEC.

IF git_zmmt0142 IS NOT INITIAL.
*&---------------------------------------------------------------------*
*& DIM MATERIAL

  SELECT matnr, mtart, matkl FROM mara
    INTO TABLE @DATA(git_mara_rc)
    FOR ALL ENTRIES IN @git_zmmt0142
  WHERE matnr  = @git_zmmt0142-id_material.

  SELECT matnr, maktx  FROM makt
    INTO TABLE @DATA(git_makt_rc)
    FOR ALL ENTRIES IN @git_mara_rc
  WHERE matnr  = @git_mara_rc-matnr
  AND   spras = 'P'.

  SELECT mtart, mtbez FROM t134t
    INTO TABLE @DATA(git_t134t_rc)
    FOR ALL ENTRIES IN @git_mara
  WHERE mtart  = @git_mara-mtart
  AND   spras = 'P'.

  SELECT matkl, wgbez FROM t023t
    INTO TABLE @DATA(git_t023t_rc)
    FOR ALL ENTRIES IN @git_mara
  WHERE matkl  = @git_mara-matkl
  AND   spras = 'P'.

  LOOP AT git_mara_rc INTO DATA(gwa_mara_rc).

    READ TABLE git_makt_rc  INTO DATA(gwa_makt_rc)   WITH KEY matnr  = gwa_mara_rc-matnr.
    READ TABLE git_t134t_rc INTO DATA(gwa_t134t_rc)  WITH KEY mtart  = gwa_mara_rc-mtart.
    READ TABLE git_t023t_rc INTO DATA(gwa_t023t_rc)  WITH KEY matkl  = gwa_mara_rc-matkl.

    IF sy-subrc = 0.
      gwa_zmmt0136-mandt = sy-mandt.
      gwa_zmmt0136-id_material     =  gwa_mara_rc-matnr.
      gwa_zmmt0136-descr_material  =  gwa_makt_rc-maktx.
      gwa_zmmt0136-id_tipo         =  gwa_mara_rc-mtart.
      gwa_zmmt0136-descr_tipo      =  gwa_t134t_rc-mtbez.
      gwa_zmmt0136-id_grupo        =  gwa_mara_rc-matkl.
      gwa_zmmt0136-descr_grup      =  gwa_t023t_rc-wgbez.

      MODIFY zmmt0136 FROM gwa_zmmt0136.
      CLEAR:gwa_zmmt0136.
    ENDIF.
  ENDLOOP.

*&---------------------------------------------------------------------*
*& DIM MATERIAL+CENTRO+DEPOSITO

  SELECT matnr, werks, bstmi , plifz FROM marc
    INTO TABLE @DATA(git_marc_rc)
    FOR ALL ENTRIES IN @git_zmmt0142
  WHERE matnr  = @git_zmmt0142-id_material
    AND werks  = @git_zmmt0142-id_centro.

  SELECT matnr, werks, lgort , ersda FROM mard
    INTO TABLE @DATA(git_mard_rc)
    FOR ALL ENTRIES IN @git_zmmt0142
  WHERE matnr  =  @git_zmmt0142-id_material
    AND werks  =  @git_zmmt0142-id_centro
    AND lgort  =  @git_zmmt0142-id_deposito.


  LOOP AT git_zmmt0142 INTO DATA(gwa_zmmt0142_aux).

    READ TABLE git_marc_rc INTO DATA(gwa_marc_rc) WITH KEY matnr  = gwa_zmmt0142_aux-id_material
                                                     werks  = gwa_zmmt0142-id_centro.

    READ TABLE git_mard_rc INTO DATA(gwa_mard_rc) WITH KEY matnr  =  gwa_zmmt0142-id_material
                                                     werks  =  gwa_zmmt0142-id_centro
                                                     lgort  =  gwa_zmmt0142-id_deposito.
    IF sy-subrc = 0.
      gwa_zmmt0137-mandt               = sy-mandt.
      gwa_zmmt0137-id_centro           = gwa_marc_rc-werks.
      gwa_zmmt0137-id_material         = gwa_marc_rc-matnr.
      gwa_zmmt0137-id_deposito         = gwa_mard_rc-lgort.
      gwa_zmmt0137-lote_minimo_compra  = gwa_marc_rc-bstmi.
      gwa_zmmt0137-data_expansao       = gwa_mard_rc-ersda.
      gwa_zmmt0137-leadtime            = gwa_marc_rc-plifz.

      MODIFY zmmt0137 FROM gwa_zmmt0137.
      CLEAR:gwa_zmmt0137.
    ENDIF.
  ENDLOOP.

*&---------------------------------------------------------------------*
*& DIM_DEPOSITO

  SELECT werks, lgort, lgobe  FROM t001l
    INTO TABLE @DATA(git_t001l_rc)
    FOR ALL ENTRIES IN @git_zmmt0142
  WHERE werks  =  @git_zmmt0142-id_centro
    AND lgort  =  @git_zmmt0142-id_deposito.

  LOOP AT git_t001l_rc INTO DATA(gwa_t001l_rc).
    gwa_zmmt0138-mandt          = sy-mandt.
    gwa_zmmt0138-id_centro      = gwa_t001l_rc-werks.
    gwa_zmmt0138-id_deposito    = gwa_t001l_rc-lgort.
    gwa_zmmt0138-descr_deposito = gwa_t001l_rc-lgobe.

    MODIFY zmmt0138 FROM gwa_zmmt0138.
    CLEAR:gwa_zmmt0138.
  ENDLOOP.
  COMMIT WORK.
ENDIF.
*&---------------------------------------------------------------------*
*& Gerar Dimensões das Requisições de Compra
*&---------------------------------------------------------------------*
IF git_zmmt0143 IS NOT INITIAL.
*&---------------------------------------------------------------------*
*& DIM MATERIAL
  SELECT matnr, mtart, matkl FROM mara
    INTO TABLE @DATA(git_mara_pd)
    FOR ALL ENTRIES IN @git_zmmt0143
  WHERE matnr  = @git_zmmt0143-id_material.

  SELECT matnr, maktx  FROM makt
    INTO TABLE @DATA(git_makt_pd)
    FOR ALL ENTRIES IN @git_mara_pd
  WHERE matnr  = @git_mara_pd-matnr
  AND   spras = 'P'.

  SELECT mtart, mtbez FROM t134t
    INTO TABLE @DATA(git_t134t_pd)
    FOR ALL ENTRIES IN @git_mara_pd
  WHERE mtart  = @git_mara_pd-mtart
  AND   spras = 'P'.

  SELECT matkl, wgbez FROM t023t
    INTO TABLE @DATA(git_t023t_pd)
    FOR ALL ENTRIES IN @git_mara_pd
  WHERE matkl  = @git_mara_pd-matkl
  AND   spras = 'P'.

  LOOP AT git_mara_pd INTO DATA(gwa_mara_pd).

    READ TABLE git_makt_pd  INTO DATA(gwa_makt_pd)   WITH KEY matnr  = gwa_mara_pd-matnr.
    READ TABLE git_t134t_pd INTO DATA(gwa_t134t_pd)  WITH KEY mtart  = gwa_mara_pd-mtart.
    READ TABLE git_t023t_pd INTO DATA(gwa_t023t_pd)  WITH KEY matkl  = gwa_mara_pd-matkl.

    IF sy-subrc = 0.

      gwa_zmmt0136-mandt = sy-mandt.
      gwa_zmmt0136-id_material     =  gwa_mara_pd-matnr.
      gwa_zmmt0136-descr_material  =  gwa_makt_pd-maktx.
      gwa_zmmt0136-id_tipo         =  gwa_mara_pd-mtart.
      gwa_zmmt0136-descr_tipo      =  gwa_t134t_pd-mtbez.
      gwa_zmmt0136-id_grupo        =  gwa_mara_pd-matkl.
      gwa_zmmt0136-descr_grup      =  gwa_t023t_pd-wgbez.

      MODIFY zmmt0136 FROM gwa_zmmt0136.
      CLEAR:gwa_zmmt0136.
    ENDIF.
  ENDLOOP.
  COMMIT WORK.

*&---------------------------------------------------------------------*
*& DIM MATERIAL+CENTRO+DEPOSITO

  SELECT matnr, werks, bstmi , plifz FROM marc
    INTO TABLE @DATA(git_marc_pd)
    FOR ALL ENTRIES IN @git_zmmt0143
  WHERE matnr  = @git_zmmt0143-id_material
    AND werks  = @git_zmmt0143-id_centro.

  SELECT matnr, werks, lgort , ersda FROM mard
    INTO TABLE @DATA(git_mard_pd)
    FOR ALL ENTRIES IN @git_zmmt0143
  WHERE matnr  =  @git_zmmt0143-id_material
    AND werks  =  @git_zmmt0143-id_centro
    AND lgort  =  @git_zmmt0143-id_deposito.


  LOOP AT git_zmmt0143 INTO DATA(gwa_zmmt0143_aux).

    READ TABLE git_marc_pd INTO DATA(gwa_marc_pd) WITH KEY matnr  = gwa_zmmt0143_aux-id_material
                                                     werks  = gwa_zmmt0143-id_centro.

    READ TABLE git_mard_pd INTO DATA(gwa_mard_pd) WITH KEY matnr  =  gwa_zmmt0143-id_material
                                                     werks  =  gwa_zmmt0142-id_centro
                                                     lgort  =  gwa_zmmt0142-id_deposito.
    IF sy-subrc = 0.

      gwa_zmmt0137-mandt               = sy-mandt.
      gwa_zmmt0137-id_centro           = gwa_marc_pd-werks.
      gwa_zmmt0137-id_material         = gwa_marc_pd-matnr.
      gwa_zmmt0137-id_deposito         = gwa_mard_pd-lgort.
      gwa_zmmt0137-lote_minimo_compra  = gwa_marc_pd-bstmi.
      gwa_zmmt0137-data_expansao       = gwa_mard_pd-ersda.
      gwa_zmmt0137-leadtime            = gwa_marc_pd-plifz.

      MODIFY zmmt0137 FROM gwa_zmmt0137.
      CLEAR:gwa_zmmt0137.
    ENDIF.
  ENDLOOP.
  COMMIT WORK.

*&---------------------------------------------------------------------*
*& DIM_DEPOSITO
  SELECT werks, lgort, lgobe  FROM t001l
    INTO TABLE @DATA(git_t001l_pd)
    FOR ALL ENTRIES IN @git_zmmt0143
  WHERE werks  =  @git_zmmt0143-id_centro
    AND lgort  =  @git_zmmt0143-id_deposito.

  LOOP AT git_t001l_pd INTO DATA(gwa_t001l_pd).
    gwa_zmmt0138-mandt          = sy-mandt.
    gwa_zmmt0138-id_centro      = gwa_t001l_pd-werks.
    gwa_zmmt0138-id_deposito    = gwa_t001l_pd-lgort.
    gwa_zmmt0138-descr_deposito = gwa_t001l_pd-lgobe.

    MODIFY zmmt0138 FROM gwa_zmmt0138.
    CLEAR:gwa_zmmt0138.
  ENDLOOP.
  COMMIT WORK.
ENDIF.

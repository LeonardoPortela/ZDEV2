*&---------------------------------------------------------------------*
*& Report ZTESTELP0001
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZTESTELP0001.
TABLES:zmmt0144.

DATA:  gwa_zmmt0144 TYPE zmmt0144.

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

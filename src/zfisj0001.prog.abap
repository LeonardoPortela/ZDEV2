*&--------------------------------------------------------------------&*
*& Report Name    : JOB de Serviço de atualização de registros       *&
*& Author         : Victor Hugo                                       *&
*& Date           : 25.04.2012                                        *&
*& Funcional Area : Fiscal                                            *&
*&                                                                    *&
*&--------------------------------------------------------------------&*
REPORT  zfisj0001.
*&--------------------------------------------------------------------&*
*& TYPES
*&--------------------------------------------------------------------&*
TYPE-POOLS : tpit.
TABLES: bsid.
TYPES:
  BEGIN OF ty_zfit0026,
    zid_lanc      TYPE zfit0026-zid_lanc,
    docnum        TYPE zfit0026-docnum,
    vbeln         TYPE zfit0026-vbeln,
    seq           TYPE zfit0026-seq,
    data_venc     TYPE zfit0026-data_venc,
    moeda         TYPE zfit0026-moeda,
    mont_moeda    TYPE zfit0026-mont_moeda,
    taxa          TYPE zfit0026-taxa,
    mont_mi       TYPE zfit0026-mont_mi,
    forma_pag     TYPE zfit0026-forma_pag,
    status        TYPE zfit0026-status,
    uname         TYPE zfit0026-uname,
    data_registro TYPE zfit0026-data_registro,
    bukrs         TYPE zfit0026-bukrs,
    obj_key       TYPE zfit0026-obj_key,

  END OF ty_zfit0026.


*&--------------------------------------------------------------------&*
*& Internal Tables
*&--------------------------------------------------------------------&*
DATA: it_zfit0026 TYPE TABLE OF ty_zfit0026.

*&--------------------------------------------------------------------&*
*& Work Area
*&--------------------------------------------------------------------&*
DATA: wa_zfit0026         TYPE ty_zfit0026,
      wa_zib_contabil     TYPE zib_contabil,
      wa_zib_contabil_chv TYPE zib_contabil_chv,
      wa_zib_contabil_err TYPE zib_contabil_err.


DATA : it_errtab TYPE  tpit_t_errdoc WITH HEADER LINE,
       it_fldtab TYPE  tpit_t_fname  WITH HEADER LINE,
       it_buztab TYPE  tpit_t_buztab WITH HEADER LINE,
       w_bseg    TYPE bseg.



SELECT zid_lanc docnum vbeln seq data_venc moeda mont_moeda taxa mont_mi forma_pag status uname data_registro bukrs obj_key
  FROM zfit0026
  INTO TABLE it_zfit0026
WHERE status EQ 'P'.


LOOP AT it_zfit0026 INTO wa_zfit0026.

  SELECT SINGLE * FROM zib_contabil INTO wa_zib_contabil WHERE obj_key EQ wa_zfit0026-obj_key.

  IF ( sy-subrc EQ 0 ).


    SELECT SINGLE * FROM zib_contabil_chv INTO wa_zib_contabil_chv WHERE obj_key EQ wa_zfit0026-obj_key.

    IF ( sy-subrc EQ 0 ) AND ( wa_zib_contabil_chv-belnr IS NOT INITIAL ). "Alteração CS2017000894 Para rodar o job apenas nos Lançamentos que possuem N. de Doc na CHV

      UPDATE zfit0026 SET status = 'G'
                      docnum =  wa_zib_contabil_chv-belnr
                      WHERE obj_key EQ  wa_zfit0026-obj_key.

      CLEAR:  it_fldtab.
      it_fldtab-fname = 'ZUONR'.
      APPEND it_fldtab.

      w_bseg-zuonr = wa_zfit0026-vbeln.

      SELECT SINGLE *
       FROM bseg
      INTO CORRESPONDING FIELDS OF it_buztab
      WHERE belnr = wa_zib_contabil_chv-belnr.

      IF sy-subrc = 0.
        APPEND it_buztab.
      ENDIF.

      CALL FUNCTION 'FI_ITEMS_MASS_CHANGE'
        EXPORTING
          s_bseg     = w_bseg
        IMPORTING
          errtab     = it_errtab[]
        TABLES
          it_buztab  = it_buztab
          it_fldtab  = it_fldtab
        EXCEPTIONS
          bdc_errors = 1
          OTHERS     = 2.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno "SY-MSGTY
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ELSE.

        UPDATE bseg SET vbel2 = wa_zfit0026-vbeln
                        posn2 = '000010'
                        hzuon = wa_zfit0026-vbeln
                        WHERE belnr EQ wa_zib_contabil_chv-belnr.

*        UPDATE BSID SET vbel2 = wa_zfit0026-vbeln
*                        vpos2 = '000010'
*                        WHERE belnr EQ wa_zib_contabil_chv-belnr.


      ENDIF.

    ELSE.

      SELECT SINGLE * FROM zib_contabil_err INTO wa_zib_contabil_err WHERE obj_key EQ wa_zfit0026-obj_key.

      IF ( sy-subrc EQ 0 ).
        UPDATE zfit0026 SET status = 'E'
                        WHERE obj_key EQ  wa_zfit0026-obj_key.

      ENDIF.
    ENDIF.
  ENDIF.
  CLEAR: wa_zfit0026, wa_zib_contabil, wa_zib_contabil_chv, wa_zib_contabil_err.
ENDLOOP.

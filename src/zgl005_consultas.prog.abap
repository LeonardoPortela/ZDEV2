************************************************************************
* Responsável ...: Amaggi Exportação & Importação Ltda.                *
* Data desenv ...: 06.08.2008                                          *
* Tipo de prg ...: INCLUDE                                          *
* Objetivo    ...: Relatório para conferencia de lançamentos indevidos *
*                  nas contas com relevância para classificação de     *
*                  custos originadas em MM                             *
************************************************************************
* Data Modif    Autor                Descriçao            Request      *
************************************************************************
* 08.08.2008    Marcus Barbara       Inicio               DEVK904650   *
************************************************************************

*----------------------------------------------------------------------*
***INCLUDE ZGL005_CONSULTAS .
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  F_CONSULTA_BSEG
*&---------------------------------------------------------------------*
*       Consulta Segmento do documento contabilidade financeira        *
*----------------------------------------------------------------------*

FORM f_consulta_bseg .

  PERFORM f_mensagem USING 'Segmento do documento contabilidade financeira'.

  DATA ETL28C2R5170 TYPE TABLE OF BSEG.
DATA LT_FIELDS_L28C2R8027 TYPE FAGL_T_FIELD.
LT_FIELDS_L28C2R8027 = VALUE #( ( LINE = 'BUKRS' )
 ( LINE = 'BELNR' )
 ( LINE = 'GJAHR' )
 ( LINE = 'BUZEI' )
 ( LINE = 'SHKZG' )
 ( LINE = 'DMBTR' )
 ( LINE = 'DMBE2' )
 ( LINE = 'EBELN' )
 ( LINE = 'LIFNR' )
 ( LINE = 'EBELP' )
 ( LINE = 'MATNR' )
 ).

CALL FUNCTION 'FAGL_GET_BSEG_FOR_ALL_ENTRIES'
    EXPORTING IT_FOR_ALL_ENTRIES = IT_BSIS
              I_WHERE_CLAUSE = |BUKRS EQ IT_FOR_ALL_ENTRIES-BUKRS AND BELNR EQ IT_FOR_ALL_ENTRIES-BELNR AND GJAHR EQ IT_FOR_ALL_ENTRIES-GJAHR AND BUZEI EQ IT_FOR_ALL_ENTRIES-BUZEI|
              IT_FIELDLIST = LT_FIELDS_L28C2R8027
    IMPORTING ET_BSEG = ETL28C2R5170
    EXCEPTIONS NOT_FOUND = 1.
IF SY-SUBRC = 0 AND LINES( ETL28C2R5170 ) > 0.
  MOVE-CORRESPONDING ETL28C2R5170 TO IT_BSEG.
  SY-DBCNT = LINES( ETL28C2R5170 ).
ELSE.
  SY-SUBRC = 4.
  SY-DBCNT = 0.
ENDIF.

  SORT it_bseg BY bukrs belnr gjahr buzei.

ENDFORM.                    " F_CONSULTA_BSEG

*&---------------------------------------------------------------------*
*&      Form  F_CONSULTA_EKPO
*&---------------------------------------------------------------------*
*       Consulta Itens Item do documento de compras                    *
*----------------------------------------------------------------------*

FORM f_consulta_ekpo .

  PERFORM f_mensagem USING 'Item do documento de compras'.

  SELECT ebeln ebelp matnr matkl banfn txz01
    FROM ekpo
    INTO TABLE it_ekpo
     FOR ALL ENTRIES IN it_bseg
   WHERE ebeln EQ it_bseg-ebeln.
  SORT it_ekpo BY ebeln matnr matkl banfn.

ENDFORM.                    " F_CONSULTA_EKPO

*&---------------------------------------------------------------------*
*&      Form  F_CONSULTA_EBAN
*&---------------------------------------------------------------------*
*       Consulta Requisição de compra
*----------------------------------------------------------------------*
FORM f_consulta_eban .

  PERFORM f_mensagem USING 'Requisição de compra'.

  SELECT banfn ernam
    FROM eban
    INTO TABLE it_eban
     FOR ALL ENTRIES IN it_ekpo
   WHERE banfn EQ it_ekpo-banfn.
  SORT it_eban BY banfn ernam.

ENDFORM.                    " F_CONSULTA_EBAN

*&---------------------------------------------------------------------*
*&      Form  f_consulta_makt_ekpo
*&---------------------------------------------------------------------*
*       Consulta Textos breves de material                             *
*----------------------------------------------------------------------*
FORM f_consulta_makt_ekpo .
  PERFORM f_mensagem USING 'Textos breves de material'.

  SELECT matnr maktx
    FROM makt
    INTO TABLE it_makt
     FOR ALL ENTRIES IN it_ekpo
   WHERE matnr EQ it_ekpo-matnr
     AND spras EQ sy-langu.
  SORT it_makt BY matnr maktx.

ENDFORM.                    " f_consulta_makt_ekpo
*&---------------------------------------------------------------------*
*&      Form  F_CONSULTA_LFA1
*&---------------------------------------------------------------------*
*       Consulta Mestre de fornecedores (parte geral)                  *
*----------------------------------------------------------------------*
FORM f_consulta_lfa1 .
  PERFORM f_mensagem USING 'Mestre de fornecedores (parte geral)'.

  SELECT lifnr name1
    FROM lfa1
    INTO TABLE it_lfa1
     FOR ALL ENTRIES IN it_ekko
   WHERE lifnr EQ it_ekko-lifnr.
  SORT it_lfa1 BY lifnr name1.
ENDFORM.                    " F_CONSULTA_LFA1

*&---------------------------------------------------------------------*
*&      Form  F_CONSULTA_CSKT
*&---------------------------------------------------------------------*
*       Consulta Textos de centros de custo                            *
*----------------------------------------------------------------------*
FORM f_consulta_cskt .

  PERFORM f_mensagem USING 'Textos de centros de custo'.

  SELECT kostl ktext
    FROM cskt
    INTO TABLE it_cskt
     FOR ALL ENTRIES IN it_bsis
   WHERE kostl EQ it_bsis-kostl
     AND spras EQ sy-langu.
  SORT it_cskt BY kostl ktext.

ENDFORM.                    " F_CONSULTA_CSKT

*&---------------------------------------------------------------------*
*&      Form  F_CONSULTA_T001
*&---------------------------------------------------------------------*
*       Consulta Empresas
*----------------------------------------------------------------------*
FORM f_consulta_t001 .
  PERFORM f_mensagem USING 'Empresas'.

  SELECT bukrs butxt
    FROM t001
    INTO TABLE it_t001
     FOR ALL ENTRIES IN it_bsis
   WHERE bukrs EQ it_bsis-bukrs.
  SORT it_t001 BY bukrs butxt.

ENDFORM.                    " F_CONSULTA_T001

*&---------------------------------------------------------------------*
*&      Form  F_CONSULTA_T001W
*&---------------------------------------------------------------------*
*       Consulta Centros/filiais
*----------------------------------------------------------------------*
FORM f_consulta_t001w .
  PERFORM f_mensagem USING 'Centros/filiais'.
  SELECT werks name1
    FROM t001w
    INTO TABLE it_t001w
     FOR ALL ENTRIES IN it_bsis
   WHERE werks EQ it_bsis-gsber.
  SORT it_t001w BY werks name1.
ENDFORM.                    " F_CONSULTA_T001W

*&---------------------------------------------------------------------*
*&      Form  F_CONSULTA_SKAT
*&---------------------------------------------------------------------*
*       Consulta Mestre de contas do Razão (plano de contas: denominação) *
*----------------------------------------------------------------------*
FORM f_consulta_skat .

  PERFORM f_mensagem USING 'Mestre de contas do Razão (plano de contas: denominação)'.

  SELECT saknr txt20
    FROM skat
    INTO TABLE it_skat
     FOR ALL ENTRIES IN it_bsis
   WHERE saknr EQ it_bsis-hkont
     AND spras EQ sy-langu.
  SORT it_skat BY saknr.

ENDFORM.                    " F_CONSULTA_SKAT

*&---------------------------------------------------------------------*
*&      Form  F_CONSULTA_BKPF
*&---------------------------------------------------------------------*
*       Consulta Cabeçalho do documento contábil                       *
*----------------------------------------------------------------------*
FORM f_consulta_bkpf .

  PERFORM f_mensagem USING 'Cabeçalho do documento contábil'.

  SELECT bukrs belnr gjahr awkey
    FROM bkpf
    INTO TABLE it_bkpf2
     FOR ALL ENTRIES IN it_bsis
   WHERE bukrs EQ it_bsis-bukrs
     AND belnr EQ it_bsis-belnr
     AND gjahr EQ it_bsis-gjahr.

  SORT it_bkpf2 BY bukrs belnr gjahr awkey.

  CLEAR: wa_bkpf.

  LOOP AT it_bkpf2 INTO wa_bkpf.
    wa_bkpf-bukrs = wa_bkpf-bukrs.
    wa_bkpf-belnr = wa_bkpf-belnr.
    wa_bkpf-gjahr = wa_bkpf-gjahr.
    wa_bkpf-awkey = wa_bkpf-awkey.
    wa_bkpf-awk10 = wa_bkpf-awkey(10).
    wa_bkpf-mjahr = wa_bkpf-awkey+10(4).
    IF wa_bkpf-mjahr IS NOT INITIAL.
      APPEND wa_bkpf TO it_bkpf.
    ENDIF.
    CLEAR: wa_bkpf.
  ENDLOOP.

ENDFORM.                    " F_CONSULTA_BKPF

*&---------------------------------------------------------------------*
*&      Form  F_CONSULTA_MSEG
*&---------------------------------------------------------------------*
*       Consulta de Segmento de documento - material
*----------------------------------------------------------------------*
FORM f_consulta_mseg.

  PERFORM f_mensagem USING 'Segmento de documento - material'.

  SELECT mblnr mjahr matnr rsnum
    FROM mseg
    INTO TABLE it_mseg
     FOR ALL ENTRIES IN it_bkpf
   WHERE mblnr EQ it_bkpf-awk10
     AND mjahr EQ it_bkpf-mjahr.

  SORT it_mseg BY mblnr mjahr matnr rsnum.

ENDFORM.                    " F_CONSULTA_MSEG

*&---------------------------------------------------------------------*
*&      Form  f_consulta_resb
*&---------------------------------------------------------------------*
*       Consulta Reserva/necessidade dependente
*----------------------------------------------------------------------*
FORM f_consulta_resb.

  PERFORM f_mensagem USING 'Reserva/necessidade dependente'.

  SELECT rsnum matkl
    FROM resb
    INTO TABLE it_resb
     FOR ALL ENTRIES IN it_mseg
   WHERE rsnum EQ it_mseg-rsnum.

  SORT it_resb BY rsnum matkl.

ENDFORM.                    " f_consulta_resb

*&---------------------------------------------------------------------*
*&      Form  F_CONSULTA_RKPF
*&---------------------------------------------------------------------*
*       Consulta de Cabeçalho do documento da reserva
*----------------------------------------------------------------------*
FORM f_consulta_rkpf .

  PERFORM f_mensagem USING 'Cabeçalho do documento da reserva'.

  SELECT rsnum usnam
    FROM rkpf
    INTO TABLE it_rkpf
     FOR ALL ENTRIES IN it_mseg
   WHERE rsnum EQ it_mseg-rsnum.

  SORT it_rkpf BY rsnum usnam.

ENDFORM.                    " F_CONSULTA_RKPF

*&---------------------------------------------------------------------*
*&      Form  F_CONSULTA_MAKT_MSEG
*&---------------------------------------------------------------------*
*       Consulta Textos breves de material
*----------------------------------------------------------------------*
FORM f_consulta_makt_mseg .

  PERFORM f_mensagem USING 'Textos breves de material'.

  SELECT matnr maktx
    FROM makt
    INTO TABLE it_makt
     FOR ALL ENTRIES IN it_mseg
   WHERE matnr EQ it_mseg-matnr
     AND spras EQ sy-langu.
  SORT it_makt BY matnr maktx.

ENDFORM.                    " F_CONSULTA_MAKT_MSEG

*&---------------------------------------------------------------------*
*&      Form  F_CONSULTA_T023T_EKPO
*&---------------------------------------------------------------------*
*       Consulta Denominações para grupos de mercadoria
*----------------------------------------------------------------------*
FORM f_consulta_t023t_ekpo .

  PERFORM f_mensagem USING 'Denominações para grupos de mercadoria'.

  SELECT matkl wgbez60
    FROM t023t
    INTO TABLE it_t023t
     FOR ALL ENTRIES IN it_ekpo
   WHERE matkl EQ it_ekpo-matkl
     AND spras EQ sy-langu.
  SORT it_t023t BY matkl wgbez60.

ENDFORM.                    " F_CONSULTA_T023T_EKPO

*&---------------------------------------------------------------------*
*&      Form  F_CONSULTA_T023T_RESB
*&---------------------------------------------------------------------*
*       Consulta Denominações para grupos de mercadoria
*----------------------------------------------------------------------*
FORM f_consulta_t023t_resb .

  PERFORM f_mensagem USING 'Denominações para grupos de mercadoria'.

  SELECT matkl wgbez60
    FROM t023t
    INTO TABLE it_t023t
     FOR ALL ENTRIES IN it_resb
   WHERE matkl EQ it_resb-matkl
     AND spras EQ sy-langu.
  SORT it_t023t BY matkl wgbez60.

ENDFORM.                    " F_CONSULTA_T023T_RESB
*&---------------------------------------------------------------------*
*&      Form  F_CONSULTA_EKKO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_consulta_ekko .

  PERFORM f_mensagem USING 'Denominações para grupos de mercadoria'.

  SELECT ebeln lifnr
    FROM ekko
    INTO TABLE it_ekko
     FOR ALL ENTRIES IN it_bseg
   WHERE ebeln EQ it_bseg-ebeln.
  SORT it_ekko BY ebeln lifnr.

ENDFORM.                    " F_CONSULTA_EKKO

************************************************************************
* A M A G G I  E X P O R T A Ç Ã  O  E  I M P O R T A Ç Ã O  L T D A.  *
*                                                                      *
************************************************************************
* Responsável ...: Amaggi Exportação & Importação Ltda                 *
* Data desenv ...: 28.01.2009                                          *
* Tipo de prg ...: executável                                          *
* Objetivo    ...: Atualização de campo                                *
*                                                                      *
************************************************************************
* Data Modif    Autor                Descriçao            Request      *
************************************************************************
* 28.01.2009    Marcus.Barbara       Criação              DEVK905454   *
* 29.01.2009    Marcus.Barbara       Alteração            DEVK905459   *
*                                                                      *
************************************************************************

REPORT  ZMMT0004.

DATA: BEGIN OF WA_EKKO,
         EBELN LIKE EKKO-EBELN,
      END OF WA_EKKO,

      BEGIN OF WA_EKPO,
         EBELN  LIKE EKPO-EBELN,
         EBELP  LIKE EKPO-EBELP,
         PACKNO LIKE EKPO-PACKNO,
      END OF WA_EKPO.

DATA: BEGIN OF WA_ZMMT0004.
        INCLUDE STRUCTURE ZMMT0004.
DATA: END   OF WA_ZMMT0004.

DATA: IT_EKKO LIKE STANDARD TABLE OF WA_EKKO,
      IT_EKPO LIKE STANDARD TABLE OF WA_EKPO,
      IT_ZMMT0004 LIKE STANDARD TABLE OF WA_ZMMT0004.

SELECT EBELN
  INTO TABLE IT_EKKO
  FROM EKKO
  WHERE BSTYP EQ 'K'.

SELECT EBELN EBELP PACKNO
  INTO TABLE IT_EKPO
  FROM EKPO
   FOR ALL ENTRIES IN IT_EKKO
 WHERE EBELN EQ IT_EKKO-EBELN
   AND KNTTP EQ 'F'
   AND PSTYP EQ '9'.

SELECT *
  INTO TABLE IT_ZMMT0004
  FROM ZMMT0004
   FOR ALL ENTRIES IN IT_EKPO
 WHERE EBELN EQ IT_EKPO-EBELN
   AND EBELP EQ IT_EKPO-EBELP.

SORT: IT_EKPO BY EBELN EBELP,
      IT_ZMMT0004 BY EBELN EBELP.

LOOP AT IT_ZMMT0004 INTO WA_ZMMT0004.
  READ TABLE IT_EKPO INTO WA_EKPO
        WITH KEY EBELN = WA_ZMMT0004-EBELN
                 EBELP = WA_ZMMT0004-EBELP
                 BINARY SEARCH.
  IF SY-SUBRC EQ 0.
    UPDATE ZMMT0004 SET PACKNO = WA_EKPO-PACKNO
     WHERE MATNR EQ WA_ZMMT0004-MATNR
       AND WERKS EQ WA_ZMMT0004-WERKS
       AND EBELN EQ WA_EKPO-EBELN
       AND EBELP EQ WA_EKPO-EBELP.
  ENDIF.
ENDLOOP.

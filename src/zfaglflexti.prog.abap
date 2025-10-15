*&---------------------------------------------------------------------*
*& Report  ZFAGLFLEXTI
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  zfaglflexti.


*----------------------------------------------------------------------*
* Definição de Parâmetros e Opções de Seleção                          *
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-s01.
PARAMETERS: pryear    LIKE faglflexa-ryear,
            pdocnr    LIKE faglflexa-docnr,
            prldnr    LIKE faglflexa-rldnr,
            prbukrs   LIKE faglflexa-rbukrs,
            pdocln    LIKE faglflexa-docln,
            pactiv    LIKE faglflexa-activ,
            prtcur    LIKE faglflexa-rtcur,
            pawtyp    LIKE faglflexa-awtyp,
            prrcty    LIKE faglflexa-rrcty,
            prvers    LIKE faglflexa-rvers,
            pracct    LIKE faglflexa-racct,
            pkokrs    LIKE faglflexa-kokrs,
            psegment  LIKE faglflexa-segment,
            ptsl      LIKE faglflexa-tsl,
            phsl      LIKE faglflexa-hsl,
            pksl      LIKE faglflexa-ksl,
            posl      LIKE faglflexa-osl,
            pwsl      LIKE faglflexa-wsl,
            pdrcrk    LIKE faglflexa-drcrk,
            ppoper    LIKE faglflexa-poper,
            prwcur    LIKE faglflexa-rwcur,
            pgjahr    LIKE faglflexa-gjahr,
            pbudat    LIKE faglflexa-budat,
            pbelnr    LIKE faglflexa-belnr,
            pbuzei    LIKE faglflexa-buzei,
            pbschl    LIKE faglflexa-bschl,
            plinetyp  LIKE faglflexa-linetype,
            pxsplit   LIKE faglflexa-xsplitmod,
            pusnam    LIKE faglflexa-usnam,
            ptimes    LIKE faglflexa-timestamp.
SELECTION-SCREEN END OF BLOCK b1.


START-OF-SELECTION.

  DATA: BEGIN OF WA_FAGL.
          INCLUDE STRUCTURE faglflexa.
  DATA: END OF WA_FAGL.

  WA_FAGL-ryear     = pryear.
  WA_FAGL-DOCNR     = pdocnr.
  WA_FAGL-rldnr     = prldnr.
  WA_FAGL-rbukrs    = prbukrs.
  WA_FAGL-docln     = pdocln.
  WA_FAGL-activ     = pactiv.
  WA_FAGL-rtcur     = prtcur.
  WA_FAGL-awtyp     = pawtyp.
  WA_FAGL-rrcty     = prrcty.
  WA_FAGL-rvers     = prvers.
  WA_FAGL-racct     = pracct.
  WA_FAGL-kokrs     = pkokrs.
  WA_FAGL-segment   = psegment.
  WA_FAGL-tsl       = ptsl.
  WA_FAGL-hsl       = phsl.
  WA_FAGL-ksl       = pksl.
  WA_FAGL-osl       = posl.
  WA_FAGL-wsl       = pwsl.
  WA_FAGL-drcrk     = pdrcrk.
  WA_FAGL-poper     = ppoper.
  WA_FAGL-rwcur     = prwcur.
  WA_FAGL-gjahr     = pgjahr.
  WA_FAGL-budat     = pbudat.
  WA_FAGL-belnr     = pbelnr.
  WA_FAGL-buzei     = pbuzei.
  WA_FAGL-bschl     = pbschl.
  WA_FAGL-linetype  = plinetyp.
  WA_FAGL-xsplitmod = pxsplit.
  WA_FAGL-usnam     = pusnam.
  WA_FAGL-timestamp = ptimes.

  INSERT into faglflexa values WA_FAGL.

  MESSAGE 'Registro Inserido!' TYPE 'I'.

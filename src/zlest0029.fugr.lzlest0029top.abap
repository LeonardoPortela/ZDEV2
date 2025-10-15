*---------------------------------------------------------------------*
*    generated viewmaintenance function pool top
*---------------------------------------------------------------------*
FUNCTION-POOL zlest0029                  MESSAGE-ID sv.

INCLUDE lsvimdat                                . "general data decl.
INCLUDE lzlest0029t00                           . "view rel. data dcl.

DATA: BEGIN OF yt_dele OCCURS 0,
        codposto   LIKE zlest0029-codposto,
        codtrp     LIKE zlest0029-codtrp,
        numseqn    LIKE zlest0029-numseqn,
        newseqn    LIKE zlest0029-numseqn,
      END OF yt_dele.

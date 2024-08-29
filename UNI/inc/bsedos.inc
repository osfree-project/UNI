;*****************************************************************************
;    bsedos.inc
;    (C) 2004-2024 osFree project
;
;    WARNING! Automaticaly generated file! Don't edit it manually!
;*****************************************************************************

		ifndef __BSEDOS_INC__
__BSEDOS_INC__	EQU	1

CCHMAXPATH EQU	260
CCHMAXPATHCOMP EQU	256
	ifdef INCL_DOS
  	include dos.inc
	endif

INCL_DOSDATETIME	EQU	1
	ifdef INCL_DOSDATETIME
  	include dosdatetime.inc
	endif

INCL_DOSDEVICES	EQU	1
	ifdef INCL_DOSDEVICES
  	include dosdevices.inc
	endif

INCL_DOSEXCEPTIONS	EQU	1
	ifdef INCL_DOSEXCEPTIONS
  	include dosexceptions.inc
	endif

INCL_DOSFILEMGR	EQU	1
	ifdef INCL_DOSFILEMGR
  	include dosfilemgr.inc
	endif

INCL_DOSMEMMGR	EQU	1
	ifdef INCL_DOSMEMMGR
  	include dosmemmgr.inc
	endif

INCL_DOSMISC	EQU	1
	ifdef INCL_DOSMISC
  	include dosmisc.inc
	endif

INCL_DOSMODULEMGR	EQU	1
	ifdef INCL_DOSMODULEMGR
  	include dosmodulemgr.inc
	endif

	ifdef INCL_DOSMONITORS
  	include dosmonitors.inc
	endif

	ifdef INCL_DOSMVDM
  	include dosmvdm.inc
	endif

	ifdef INCL_DOSNLS
  	include dosnls.inc
	endif

INCL_DOSNMPIPES	EQU	1
	ifdef INCL_DOSNMPIPES
  	include dosnmpipes.inc
	endif

INCL_DOSPROCESS	EQU	1
	ifdef INCL_DOSPROCESS
  	include dosprocess.inc
	endif

	ifdef INCL_DOSPROFILE
  	include dosprofile.inc
	endif

INCL_DOSQUEUES	EQU	1
	ifdef INCL_DOSQUEUES
  	include dosqueues.inc
	endif

INCL_DOSRESOURCES	EQU	1
	ifdef INCL_DOSRESOURCES
  	include dosresources.inc
	endif

INCL_DOSSEMAPHORES	EQU	1
	ifdef INCL_DOSSEMAPHORES
  	include dossemaphores.inc
	endif

INCL_DOSSESMGR	EQU	1
	ifdef INCL_DOSSESMGR
  	include dossesmgr.inc
	endif

	ifdef INCL_DOSMEMMGRCOMMON
  	include dosmemmgrcommon.inc
	endif

	ifdef INCL_DOSPROCESSCOMMON
  	include dosprocesscommon.inc
	endif

INCL_DOSFILEMGRCOMMON	EQU	1
	ifdef INCL_DOSFILEMGRCOMMON
  	include dosfilemgrcommon.inc
	endif

	ifdef INCL_DOSSEMAPHORESCOMMON
  	include dossemaphorescommon.inc
	endif

INCL_DOSRESOURCESCOMMON	EQU	1
	ifdef INCL_DOSRESOURCESCOMMON
  	include dosresourcescommon.inc
	endif

	ifdef INCL_DOSSESMGRFILEMGR
  	include dossesmgrfilemgr.inc
	endif

	ifdef INCL_DOSSPINLOCK
  	include dosspinlock.inc
	endif

	ifdef INCL_BDCALLS
  	include bdcalls.inc
	endif



	endif	; __BSEDOS_INC__

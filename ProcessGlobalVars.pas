{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

  Process-Global Variables

  

  Version 1.0 (2024-__-__)

  Last change 2024-__-__

  ©2024 František Milt

  Contacts:
    František Milt: frantisek.milt@gmail.com

  Support:
    If you find this code useful, please consider supporting its author(s) by
    making a small donation using the following link(s):

      https://www.paypal.me/FMilt

  Changelog:
    For detailed changelog and history please refer to this git repository:

      github.com/TheLazyTomcat/Lib.ProcessGlobalVars

  Dependencies:
    Adler32       - github.com/TheLazyTomcat/Lib.Adler32
  * AuxExceptions - github.com/TheLazyTomcat/Lib.AuxExceptions
    AuxMath       - github.com/TheLazyTomcat/Lib.AuxMath
    AuxTypes      - github.com/TheLazyTomcat/Lib.AuxTypes
    StrRect       - github.com/TheLazyTomcat/Lib.StrRect

  Library AuxExceptions is required only when rebasing local exception classes
  (see symbol ProcessGlobalVars_UseAuxExceptions for details).

  Library AuxExceptions might also be required as an indirect dependency.

  Indirect dependencies:
    AuxClasses         - github.com/TheLazyTomcat/Lib.AuxClasses
    HashBase           - github.com/TheLazyTomcat/Lib.HashBase
    SimpleCPUID        - github.com/TheLazyTomcat/Lib.SimpleCPUID
    StaticMemoryStream - github.com/TheLazyTomcat/Lib.StaticMemoryStream
    UInt64Utils        - github.com/TheLazyTomcat/Lib.UInt64Utils
    WinFileInfo        - github.com/TheLazyTomcat/Lib.WinFileInfo

===============================================================================}
unit ProcessGlobalVars;
{
  ProcessGlobalVars_UseAuxExceptions

  If you want library-specific exceptions to be based on more advanced classes
  provided by AuxExceptions library instead of basic Exception class, and don't
  want to or cannot change code in this unit, you can define global symbol
  ProcessGlobalVars_UseAuxExceptions to achieve this.
}
{$IF Defined(ProcessGlobalVars_UseAuxExceptions)}
  {$DEFINE UseAuxExceptions}
{$IFEND}

//------------------------------------------------------------------------------

{$IF defined(CPU64) or defined(CPU64BITS)}
  {$DEFINE CPU64bit}
{$ELSEIF defined(CPU16)}
  {$MESSAGE FATAL '16bit CPU not supported'}
{$ELSE}
  {$DEFINE CPU32bit}
{$IFEND}

{$IF Defined(WINDOWS) or Defined(MSWINDOWS)}
  {$DEFINE Windows}
{$ELSEIF Defined(LINUX) and Defined(FPC)}
  {$DEFINE Linux}
{$ELSE}
  {$MESSAGE FATAL 'Unsupported operating system.'}
{$IFEND}  

{$IFDEF FPC}
  {$MODE ObjFPC}
  {$DEFINE FPC_DisableWarns}
  {$MACRO ON}
  {$INLINE ON}
  {$DEFINE CanInline}
{$ELSE}
  {$IF CompilerVersion >= 17} // Delphi 2005+
    {$DEFINE CanInline}
  {$ELSE}
    {$UNDEF CanInline}
  {$IFEND}
{$ENDIF}
{$H+}

interface

uses
  SysUtils,
  AuxTypes {$IFDEF UseAuxExceptions}, AuxExceptions{$ENDIF};

{===============================================================================
    Library-specific exceptions
===============================================================================}
type
  EPGVException = class({$IFDEF UseAuxExceptions}EAEGeneralException{$ELSE}Exception{$ENDIF});

  EPGVModuleEnumerationError = class(EPGVException);
  EPGVModuleCleanupError     = class(EPGVException);

  EPGVHeapAllocationError    = class(EPGVException);

  EPGVInvalidValue           = class(EPGVException);
  EPGVUnknownVariable        = class(EPGVException);
  EPGVDuplicateVariable      = class(EPGVException);

  EPGVMutexError  = class(EPGVException);
  EPGVSystemError = class(EPGVException);

{===============================================================================
--------------------------------------------------------------------------------
                        PGV public interface declaration
--------------------------------------------------------------------------------
===============================================================================}
type
  TPGVIdentifier = UInt32;

  TPGVIdentifierArray = array of TPGVIdentifier;  // used in enumeration

  TPGVVariable = PPointer;

//------------------------------------------------------------------------------

Function GlobVarTranslateIdentifier(const Identifier: String): TPGVIdentifier;{$IFDEF CanInline} inline;{$ENDIF}

//------------------------------------------------------------------------------

procedure GlobVarLock;{$IFDEF CanInline} inline;{$ENDIF}
procedure GlobVarUnlock;{$IFDEF CanInline} inline;{$ENDIF}

Function GlobVarCount: Integer;

Function GlobVarMemory(IncludeOverhead: Boolean = False): TMemSize;

Function GlobVarEnumerate: TPGVIdentifierArray;

//------------------------------------------------------------------------------

Function GlobVarExists(Identifier: TPGVIdentifier): Boolean; overload;
Function GlobVarExists(const Identifier: String): Boolean; overload;{$IFDEF CanInline} inline;{$ENDIF}

Function GlobVarFind(Identifier: TPGVIdentifier; out Variable: TPGVVariable): Boolean; overload;
Function GlobVarFind(const Identifier: String; out Variable: TPGVVariable): Boolean; overload;{$IFDEF CanInline} inline;{$ENDIF}

Function GlobVarGet(Identifier: TPGVIdentifier): TPGVVariable; overload;
Function GlobVarGet(const Identifier: String): TPGVVariable; overload;

//------------------------------------------------------------------------------

Function GlobVarSize(Identifier: TPGVIdentifier): TMemSize; overload;
Function GlobVarSize(const Identifier: String): TMemSize; overload;

Function GlobVarHeapStored(Identifier: TPGVIdentifier): Boolean; overload;
Function GlobVarHeapStored(const Identifier: String): Boolean; overload;

//------------------------------------------------------------------------------

Function GlobVarAllocate(Identifier: TPGVIdentifier; Size: TMemSize): TPGVVariable; overload;
Function GlobVarAllocate(const Identifier: String; Size: TMemSize): TPGVVariable; overload;

Function GlobVarAlloc(Identifier: TPGVIdentifier; Size: TMemSize): TPGVVariable; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function GlobVarAlloc(const Identifier: String; Size: TMemSize): TPGVVariable; overload;{$IFDEF CanInline} inline;{$ENDIF}

Function GlobVarReallocate(Identifier: TPGVIdentifier; NewSize: TMemSize): TPGVVariable; overload;
Function GlobVarReallocate(const Identifier: String; NewSize: TMemSize): TPGVVariable; overload;{$IFDEF CanInline} inline;{$ENDIF}

Function GlobVarRealloc(Identifier: TPGVIdentifier; NewSize: TMemSize): TPGVVariable; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function GlobVarRealloc(const Identifier: String; NewSize: TMemSize): TPGVVariable; overload;{$IFDEF CanInline} inline;{$ENDIF}

procedure GlobVarFree(Identifier: TPGVIdentifier); overload;
procedure GlobVarFree(const Identifier: String); overload;

//------------------------------------------------------------------------------

Function GlobVarStore(Identifier: TPGVIdentifier; const Buffer; Count: TMemSize): TMemSize; overload;
Function GlobVarStore(const Identifier: String; const Buffer; Count: TMemSize): TMemSize; overload;

Function GlobVarLoad(Identifier: TPGVIdentifier; out Buffer; Count: TMemSize): TMemSize; overload;
Function GlobVarLoad(const Identifier: String; out Buffer; Count: TMemSize): TMemSize; overload;

//------------------------------------------------------------------------------

Function GlobVarTryGet(Identifier: TPGVIdentifier; out Variable: TPGVVariable; out Size: TMemSize): Boolean; overload;
Function GlobVarTryGet(const Identifier: String; out Variable: TPGVVariable; out Size: TMemSize): Boolean; overload;{$IFDEF CanInline} inline;{$ENDIF}

implementation

uses
  {$IFDEF Windows}Windows,{$ELSE}UnixType, BaseUnix,{$ENDIF}
  Adler32, AuxMath, StrRect;

{$IFDEF FPC_DisableWarns}
  {$DEFINE FPCDWM}
  {$DEFINE W4055:={$WARN 4055 OFF}} // Conversion between ordinals and pointers is not portable
{$ENDIF}

{===============================================================================
--------------------------------------------------------------------------------
                           PGV internal implementation
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    PGV internal implementation - constants and types
===============================================================================}
type
  TPGVHead = packed record
    RefCount:     Integer;
    Flags:        UInt32;
  {$IFDEF Windows}
    Lock:         TRTLCriticalSection;
  {$ELSE}
    Lock:         pthread_mutex_t;
    Allocator:    record
      LibHandle:    Pointer;
      AllocFunc:    Function(size: size_t): Pointer; cdecl;
      ReallocFunc:  Function(ptr: Pointer; size: size_t): Pointer; cdecl;
      FreeFunc:     procedure(ptr: Pointer); cdecl;
    end;
  {$ENDIF}
    FirstSegment: Pointer;
    LastSegment:  Pointer;
  end;
  PPGVHead = ^TPGVHead;

//------------------------------------------------------------------------------
type
  TPGVSegmentHead = packed record
    Flags:        UInt32;
    AllocCount:   Integer;
    NextSegment:  Pointer;
    PrevSegment:  Pointer;
  end;

  TPGVSegmentEntry = packed record
    Identifier: TPGVIdentifier;
    Flags:      UInt32;
    Address:    Pointer;
    Size:       TMemSize;    
  end;
  PPGVSegmentEntry = ^TPGVSegmentEntry;

const
  // SEFLAG = segment entry flag
  PGV_SEFLAG_USED        = $00000001;
  PGV_SEFLAG_REALLOCATED = $00000002;

  PGV_SEFLAG_SMLSIZE_MASK  = $F0000000;
  PGV_SEFLAG_SMLSIZE_SMASK = $0000000F;
  PGV_SEFLAG_SMLSIZE_SHIFT = 28;

//------------------------------------------------------------------------------
const  
  PGV_SEGMENT_SIZE = 4096;  // one memory page, usually

  // helper constants (so that further declarations are shorter)
  PGV_SEGMENT_ENTRYCOUNT  = (PGV_SEGMENT_SIZE - SizeOf(TPGVSegmentHead)) div SizeOf(TPGVSegmentEntry);
  PGV_SEGMENT_ENTRIESSIZE = PGV_SEGMENT_ENTRYCOUNT * SizeOf(TPGVSegmentEntry);
  PGV_SEGMENT_PADDINGSIZE = PGV_SEGMENT_SIZE - PGV_SEGMENT_ENTRIESSIZE - SizeOf(TPGVSegmentHead);

type
  TPGVSegment = packed record
    Head:     TPGVSegmentHead;
    // add padding to ensure the type has exactly segment-size bytes
  {$IF PGV_SEGMENT_PADDINGSIZE > 0}
    Padding:  packed array[0..Pred(PGV_SEGMENT_PADDINGSIZE)] of Byte;
  {$IFEND}
    Entries:  packed array[0..Pred(PGV_SEGMENT_ENTRYCOUNT)] of TPGVSegmentEntry;
  end;
  PPGVSegment = ^TPGVSegment;

{$IF SizeOf(TPGVSegment) <> PGV_SEGMENT_SIZE}
  {$MESSAGE FATAL 'Invalid size of type TPGVSegment.'}
{$IFEND}

//------------------------------------------------------------------------------
var
  // main global variable
  VAR_HeadPtr:  PPGVHead = nil;

{===============================================================================
    PGV internal implementation - thread protection
===============================================================================}
{$IFDEF Windows}

Function TryThreadLock: Boolean;
begin
Result := TryEnterCriticalSection(VAR_HeadPtr^.Lock);
end;

//------------------------------------------------------------------------------

procedure ThreadLock;
begin
EnterCriticalSection(VAR_HeadPtr^.Lock);
end;

//------------------------------------------------------------------------------

procedure ThreadUnlock;
begin
LeaveCriticalSection(VAR_HeadPtr^.Lock);
end;

{$ELSE}//=======================================================================
type
  pthread_mutexattr_p = ^pthread_mutexattr_t;
  pthread_mutex_p = ^pthread_mutex_t;

const
  PTHREAD_MUTEX_RECURSIVE = 1;
  PTHREAD_MUTEX_ROBUST    = 1;

Function pthread_mutexattr_init(attr: pthread_mutexattr_p): cint; cdecl; external;
Function pthread_mutexattr_destroy(attr: pthread_mutexattr_p): cint; cdecl; external;
Function pthread_mutexattr_settype(attr: pthread_mutexattr_p; _type: cint): cint; cdecl; external;
Function pthread_mutexattr_setrobust(attr: pthread_mutexattr_p; robustness: cint): cint; cdecl; external;

Function pthread_mutex_init(mutex: pthread_mutex_p; attr: pthread_mutexattr_p): cint; cdecl; external;
Function pthread_mutex_destroy(mutex: pthread_mutex_p): cint; cdecl; external;

Function pthread_mutex_trylock(mutex: pthread_mutex_p): cint; cdecl; external;
Function pthread_mutex_lock(mutex: pthread_mutex_p): cint; cdecl; external;
Function pthread_mutex_unlock(mutex: pthread_mutex_p): cint; cdecl; external;
Function pthread_mutex_consistent(mutex: pthread_mutex_p): cint; cdecl; external;

//------------------------------------------------------------------------------
threadvar
  ThrErrorCode: cInt;

Function PThrResChk(RetVal: cInt): Boolean;
begin
Result := RetVal = 0;
If Result then
  ThrErrorCode := 0
else
  ThrErrorCode := RetVal;
end;

//------------------------------------------------------------------------------

procedure ThreadLockInit;
var
  MutexAttr:  pthread_mutexattr_t;
begin
If PThrResChk(pthread_mutexattr_init(@MutexAttr)) then
  try
    // make the mutex recursive and robust (it does not need to be process-shared)
    If not PThrResChk(pthread_mutexattr_settype(@MutexAttr,PTHREAD_MUTEX_RECURSIVE)) then
      raise EPGVMutexError.CreateFmt('ThreadLockInit: Failed to set mutex attribute type (%d).',[ThrErrorCode]);
    If not PThrResChk(pthread_mutexattr_setrobust(@MutexAttr,PTHREAD_MUTEX_ROBUST)) then
      raise EPGVMutexError.CreateFmt('ThreadLockInit: Failed to set mutex attribute robust (%d).',[ThrErrorCode]);
    If not PThrResChk(pthread_mutex_init(@VAR_HeadPtr^.Lock,@MutexAttr)) then
      raise EPGVMutexError.CreateFmt('ThreadLockInit: Failed to init mutex (%d).',[ThrErrorCode]);
  finally
    pthread_mutexattr_destroy(@MutexAttr);
  end
else raise EPGVMutexError.CreateFmt('ThreadLockInit: Failed to init mutex attributes (%d).',[ThrErrorCode]);
end;

//------------------------------------------------------------------------------

procedure ThreadLockFinal;
begin
If not PThrResChk(pthread_mutex_destroy(@VAR_HeadPtr^.Lock)) then
  raise EPGVMutexError.CreateFmt('ThreadLockFinal: Failed to destroy mutex (%d).',[ThrErrorCode]);
end;

//------------------------------------------------------------------------------

Function TryThreadLock: Boolean;
var
  RetVal: cInt;
begin
RetVal := pthread_mutex_trylock(@VAR_HeadPtr^.Lock);
If RetVal = ESysEOWNERDEAD then
  begin
    If not PThrResChk(pthread_mutex_consistent(@VAR_HeadPtr^.Lock)) then
      raise EPGVMutexError.CreateFmt('TryThreadLock: Failed to make mutex consistent (%d).',[ThrErrorCode]);
    Result := True;
  end
else
  begin
    Result := PThrResChk(RetVal);
    If not Result and (RetVal <> ESysEBUSY) then
      raise EPGVMutexError.CreateFmt('TryThreadLock: Failed to try-lock mutex (%d).',[ThrErrorCode]);
  end;
end;

//------------------------------------------------------------------------------

procedure ThreadLock;
var
  RetVal: cInt;
begin
RetVal := pthread_mutex_lock(@VAR_HeadPtr^.Lock);
If RetVal = ESysEOWNERDEAD then
  begin
    If not PThrResChk(pthread_mutex_consistent(@VAR_HeadPtr^.Lock)) then
      raise EPGVMutexError.CreateFmt('ThreadLock: Failed to make mutex consistent (%d).',[ThrErrorCode]);
  end
else If not PThrResChk(RetVal) then
  raise EPGVMutexError.CreateFmt('ThreadLock: Failed to lock mutex (%d).',[ThrErrorCode]);
end;

//------------------------------------------------------------------------------

procedure ThreadUnlock;
begin
If not PThrResChk(pthread_mutex_unlock(@VAR_HeadPtr^.Lock)) then
  raise EPGVMutexError.CreateFmt('ThreadUnlock: Failed to unlock mutex (%d).',[ThrErrorCode]);
end;

{$ENDIF}

{===============================================================================
    PGV internal implementation - global memory management
===============================================================================}
{$IFDEF Windows}
const
  HEAP_ZERO_MEMORY = $00000008;

//------------------------------------------------------------------------------

Function GlobalMemoryAllocate(Size: TMemSize): Pointer;
begin
Result := HeapAlloc(GetProcessHeap,HEAP_ZERO_MEMORY,Size);
If not Assigned(Result) then
  raise EPGVHeapAllocationError.Create('GlobalMemoryAllocate: Failed to allocate global memory.');
end;

//------------------------------------------------------------------------------

procedure GlobalMemoryReallocate(var Address: Pointer; NewSize: TMemSize);
begin
Address := HeapRealloc(GetProcessHeap,HEAP_ZERO_MEMORY,Address,NewSize);
If not Assigned(Address) then
  raise EPGVHeapAllocationError.Create('GlobalMemoryReallocate: Failed to reallocate global memory.');
end;

//------------------------------------------------------------------------------

procedure GlobalMemoryFree(var Address: Pointer);
begin
If not HeapFree(GetProcessHeap,0,Address) then
  raise EPGVHeapAllocationError.CreateFmt('GlobalMemoryFree: Could not free global memory (%d).',[GetLastError]);
Address := nil;
end;

{$ELSE}//=======================================================================
const
  libc = 'libc.so.6';

  RTLD_LAZY = $001;
  RTLD_NOW  = $002;

Function errno_ptr: pcint; cdecl; external name '__errno_location';

Function lin_malloc(size: size_t): Pointer; cdecl; external libc name 'malloc';
Function lin_realloc(ptr: Pointer; size: size_t): Pointer; cdecl; external libc name 'realloc';
procedure lin_free(ptr: Pointer); cdecl; external libc name 'free';

Function dlopen(filename: PChar; flags: cInt): Pointer; cdecl; external;
Function dlclose(handle: Pointer): cInt; cdecl; external;
Function dlsym(handle: Pointer; symbol: PChar): Pointer; cdecl; external;
Function dlerror: PChar; cdecl; external;

//------------------------------------------------------------------------------

procedure GlobalMemoryInit;
begin
{
  Unlike in Windows, where the memory manager is provided by the system, in
  Linux it is from glibc (a library). To ensure that everyone is using the
  same manager from the same library, we store the allocation interface (the
  pointers to functions) in global state and use it from there.

  Also, to make sure the library managing the memory is not unloaded too soon,
  we open it to increment its reference count. It is then closed when the last
  instance is finalized.
}
with VAR_HeadPtr^.Allocator do
  begin
    LibHandle := dlopen(libc,RTLD_LAZY);
    If not Assigned(LibHandle) then
      raise EPGVSystemError.CreateFmt('GlobalMemoryInit: Failed to open allocating library (%s).',[dlerror]);
    AllocFunc := @lin_malloc;
    ReallocFunc := @lin_realloc;
    FreeFunc := @lin_free;
  end;
end;

//------------------------------------------------------------------------------

procedure GlobalMemoryFinal;
begin
If dlclose(VAR_HeadPtr^.Allocator.LibHandle) <> 0 then
  raise EPGVSystemError.CreateFmt('GlobalMemoryFinal: Failed to close allocating library (%s).',[dlerror]);
end;

//------------------------------------------------------------------------------

Function GlobalMemoryAllocate(Size: TMemSize): Pointer;
begin
Result := VAR_HeadPtr^.Allocator.AllocFunc(size_t(Size));
If Assigned(Result) then
  FillChar(Result^,Size,0)
else
  raise EPGVHeapAllocationError.CreateFmt('GlobalMemoryAllocate: Failed to allocate global memory (%d).',[errno_ptr^]);
end;

//------------------------------------------------------------------------------

procedure GlobalMemoryReallocate(var Address: Pointer; NewSize: TMemSize);
begin
Address := VAR_HeadPtr^.Allocator.ReallocFunc(Address,size_t(NewSize));
If not Assigned(Address) then
  raise EPGVHeapAllocationError.CreateFmt('GlobalMemoryReallocate: Failed to reallocate global memory (%d).',[errno_ptr^]);
end;

//------------------------------------------------------------------------------

procedure GlobalMemoryFree(var Address: Pointer);
begin
VAR_HeadPtr^.Allocator.FreeFunc(Address);
Address := nil;
end;

{$ENDIF}

{===============================================================================
    PGV internal implementation - main implementation (variables management)
===============================================================================}

Function SegmentAdd: PPGVSegment;
begin
Result := GlobalMemoryAllocate(SizeOf(TPGVSegment));
If Assigned(VAR_HeadPtr^.FirstSegment) then
  begin
    // there are some segments already allocated
    Result^.Head.PrevSegment := VAR_HeadPtr^.LastSegment;
    PPGVSegment(VAR_HeadPtr^.LastSegment)^.Head.NextSegment := Result;
  end
// adding first segment
else VAR_HeadPtr^.FirstSegment := Result;
VAR_HeadPtr^.LastSegment := Result;
end;

//------------------------------------------------------------------------------

procedure SegmentRemove(Segment: PPGVSegment);
begin
If Assigned(Segment^.Head.PrevSegment) then
  PPGVSegment(Segment^.Head.PrevSegment)^.Head.NextSegment := Segment^.Head.NextSegment;
If Assigned(Segment^.Head.NextSegment) then
  PPGVSegment(Segment^.Head.NextSegment)^.Head.PrevSegment := Segment^.Head.PrevSegment;
If VAR_HeadPtr^.FirstSegment = Segment then
  VAR_HeadPtr^.FirstSegment := Segment^.Head.NextSegment;
If VAR_HeadPtr^.LastSegment = Segment then
  VAR_HeadPtr^.LastSegment := Segment^.Head.PrevSegment;
GlobalMemoryFree(Pointer(Segment));
end;

//==============================================================================

Function EntrySize(Entry: PPGVSegmentEntry): TMemSize;
begin
If (Entry^.Flags and PGV_SEFLAG_SMLSIZE_MASK) <> 0 then
  Result := TMemSize((Entry^.Flags shr PGV_SEFLAG_SMLSIZE_SHIFT) and PGV_SEFLAG_SMLSIZE_SMASK)
else
  Result := Entry^.Size;
end;

//------------------------------------------------------------------------------

Function EntryFind(Identifier: TPGVIdentifier; out Segment: PPGVSegment; out Entry: PPGVSegmentEntry): Boolean;
var
  CurrentSegment: PPGVSegment;
  i:              Integer;
begin
// lock should be acquired by now
Segment := nil;
Entry := nil;
CurrentSegment := VAR_HeadPtr^.FirstSegment;
while Assigned(CurrentSegment) and not (Assigned(Segment) and Assigned(Entry)) do
  begin
    If CurrentSegment^.Head.AllocCount > 0 then
      For i := Low(CurrentSegment^.Entries) to High(CurrentSegment^.Entries) do
        If ((CurrentSegment^.Entries[i].Flags and PGV_SEFLAG_USED) <> 0) and
          (CurrentSegment^.Entries[i].Identifier = Identifier) then
          begin
            Segment := CurrentSegment;
            Entry := Addr(CurrentSegment^.Entries[i]);
            // while cycle will break too because both Segment and Entry are assigned
            Break{For i};
          end;
    CurrentSegment := CurrentSegment^.Head.NextSegment;
  end;
Result := Assigned(Segment) and Assigned(Entry);
end;

//------------------------------------------------------------------------------

procedure EntryAllocate(Identifier: TPGVIdentifier; Size: TMemSize; out Segment: PPGVSegment; out Entry: PPGVSegmentEntry);
var
  CurrentSegment: PPGVSegment;
  i:              Integer;
begin
{
  Check whether the entry already exists should have been performed prior
  calling this function.
}
If Size <= 0 then
  raise EPGVInvalidValue.CreateFmt('EntryAllocate: Invalid size (%u).',[Size]);
Segment := nil;
Entry := nil;
// find segment with unused entry
CurrentSegment := VAR_HeadPtr^.FirstSegment;
while Assigned(CurrentSegment) and not (Assigned(Segment) and Assigned(Entry)) do
  begin
    If CurrentSegment^.Head.AllocCount < Length(CurrentSegment^.Entries) then
      For i := Low(CurrentSegment^.Entries) to High(CurrentSegment^.Entries) do
        If ((CurrentSegment^.Entries[i].Flags and PGV_SEFLAG_USED) = 0) then
          begin
            Segment := CurrentSegment;
            Entry := Addr(Segment^.Entries[i]);
            Inc(Segment^.Head.AllocCount);
            Break{For i};
          end;
    CurrentSegment := CurrentSegment^.Head.NextSegment;
  end;
// if no unused entry was found, allocate new segment and get entry there
If not Assigned(Segment) or not Assigned(Entry) then
  begin
    Segment := SegmentAdd;
    Entry := Addr(Segment^.Entries[Low(Segment^.Entries)]);
    Segment^.Head.AllocCount := 1;
  end;
// allocate the entry
Entry^.Identifier := Identifier;
Entry^.Flags := PGV_SEFLAG_USED;
If Size <= SizeOf(TMemSize) then
  begin
    // do not allocate on heap, the variable can fit into entry's Size field
    Entry^.Flags := Entry^.Flags or ((UInt32(Size) and PGV_SEFLAG_SMLSIZE_SMASK) shl PGV_SEFLAG_SMLSIZE_SHIFT);
    Entry^.Address := Addr(Entry^.Size);
    Entry^.Size := 0;
  end
else
  begin
    // variable cannot be stored in-situ, allocate on heap
    Entry^.Address := GlobalMemoryAllocate(Size);
    Entry^.Size := Size;
  end;
end;

//------------------------------------------------------------------------------

procedure EntryReallocate(Segment: PPGVSegment; Entry: PPGVSegmentEntry; NewSize: TMemSize);
var
  OldSize:  TMemSize;
begin
If NewSize <= 0 then
  raise EPGVInvalidValue.CreateFmt('EntryRealloc: Invalid new size (%u).',[NewSize]);
// check whether the entry actually belongs to that segment
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
If (PtrUInt(Entry) > PtrUInt(Segment)) and ((PtrUInt(Entry) < (PtrUInt(Segment) + PGV_SEGMENT_SIZE))) then
{$IFDEF FPCDWM}{$POP}{$ENDIF}
  begin
    OldSize := EntrySize(Entry);
    If OldSize <> NewSize then
      begin
        If (Entry^.Flags and PGV_SEFLAG_SMLSIZE_MASK) <> 0 then
          begin
            // data are currently stored in Size field
            If NewSize <= SizeOf(TMemSize) then
              begin
              {
                Data will still fit to Size field - do not move anything, just
                zero upper memory when growing.
              }
                If NewSize > OldSize then
                {$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
                  FillChar(Pointer(PtrUInt(Addr(Entry^.Size)) + PtrUInt(OldSize))^,NewSize - OldSize,0);
                {$IFDEF FPCDWM}{$POP}{$ENDIF}
                Entry^.Flags := (Entry^.Flags and not PGV_SEFLAG_SMLSIZE_MASK) or
                  ((UInt32(NewSize) and PGV_SEFLAG_SMLSIZE_SMASK) shl PGV_SEFLAG_SMLSIZE_SHIFT);
              end
            else
              begin
                // data will be move to heap
                Entry^.Flags := (Entry^.Flags and not PGV_SEFLAG_SMLSIZE_MASK);
                Entry^.Address := GlobalMemoryAllocate(NewSize);
                Move(Entry^.Size,Entry^.Address^,OldSize);
                Entry^.Size := NewSize;
              end;
          end
        else
          begin
            // data are somewhere on the heap
            If NewSize <= SizeOf(TMemSize) then
              begin
                // data will go to size field
                Move(Entry^.Address^,Entry^.Size,NewSize);
                GlobalMemoryFree(Entry^.Address);
                Entry^.Address := Addr(Entry^.Size);
                Entry^.Flags := (Entry^.Flags and not PGV_SEFLAG_SMLSIZE_MASK) or
                  ((UInt32(NewSize) and PGV_SEFLAG_SMLSIZE_SMASK) shl PGV_SEFLAG_SMLSIZE_SHIFT);
              end
            else
              begin
                // data stay on heap, just reallocate
                GlobalMemoryReallocate(Entry^.Address,NewSize);
              {$IFNDEF Windows}
                // Linux manager does not clear the newly allocated memory
                If NewSize > OldSize then
                {$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
                  FillChar(Pointer(PtrUInt(Entry^.Address) + PtrUInt(OldSize))^,NewSize - OldSize,0);
                {$IFDEF FPCDWM}{$POP}{$ENDIF}
              {$ENDIF}
                Entry^.Size := NewSize;
              end;
          end;
        Entry^.Flags := Entry^.Flags or PGV_SEFLAG_REALLOCATED;
      end;
  end
else raise EPGVInvalidValue.Create('EntryRealloc: Given entry does not belong to given segment.');
end;

//------------------------------------------------------------------------------

procedure EntryFree(Segment: PPGVSegment; Entry: PPGVSegmentEntry);
begin
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
If (PtrUInt(Entry) > PtrUInt(Segment)) and ((PtrUInt(Entry) < (PtrUInt(Segment) + PGV_SEGMENT_SIZE))) then
{$IFDEF FPCDWM}{$POP}{$ENDIF}
  begin
    // free the entry
    Entry^.Size := 0;
    // free memory only if the variable is allocated on the heap
    If (Entry^.Flags or PGV_SEFLAG_SMLSIZE_MASK) = 0 then
      GlobalMemoryFree(Entry^.Address); // sets the address to nil
    Entry^.Flags := 0;
    Entry^.Identifier := 0;
    // update segment
    Dec(Segment^.Head.AllocCount);
    If Segment^.Head.AllocCount <= 0 then
      SegmentRemove(Segment);
  end
else raise EPGVInvalidValue.Create('EntryFree: Given entry does not belong to given segment.');
end;
  
{===============================================================================
    PGV internal implementation - exports
===============================================================================}
type
{
  Type TGetHeadFunc must EXACTLY match prototype of ProcessGlobalVarsGetHead
  (or vice-versa, your choice). And do not ever change it - rather introduce
  new function if different call is needed.
}
  TGetHeadFunc = Function(Version: Int32): PPGVHead;{$IFDEF Windows} stdcall;{$ELSE} cdecl;{$ENDIF}

const
  PGV_EXPORTNAME_GETHEAD = 'ProcessGlobalVarsGetHead';

  PGV_VERSION_CURRENT = Int32(0){$IFDEF CPU64bit} or Int32(UInt32(1) shl 31){$ENDIF};

//------------------------------------------------------------------------------

Function ProcessGlobalVarsGetHead(Version: Int32): PPGVHead;{$IFDEF Windows} stdcall;{$ELSE} cdecl; public;{$ENDIF}
begin
If Version = PGV_VERSION_CURRENT then
  Result := VAR_HeadPtr
else
  Result := nil;
end;

{===============================================================================
    PGV internal implementation - module initialization
===============================================================================}
{$IFDEF Windows}
type
  TModuleArray = array of THandle;

Function EnumProcessModules(hProcess: THandle; lphModules: PHandle; cb: DWORD; lpcbNeeded: LPDWORD): BOOL; stdcall; external 'psapi.dll';

//------------------------------------------------------------------------------

Function EnumerateProcessModules: TModuleArray;
var
  BytesNeeded:  DWORD;
begin
Result := nil;
BytesNeeded := 1024 * SizeOf(THandle);
repeat
  SetLength(Result,BytesNeeded);
  If not EnumProcessModules(GetCurrentProcess,Addr(Result[Low(Result)]),Length(Result) * SizeOf(THandle),@BytesNeeded) then
    raise EPGVModuleEnumerationError.CreateFmt('EnumProcessModules: Failed to enumerate process modules (%d).',[GetLastError]);
until DWORD(Length(Result) * SizeOf(THandle)) >= BytesNeeded;
// limit length to what is really enumerated
SetLength(Result,BytesNeeded div SizeOf(THandle));
end;

{$IFNDEF FPC}
//------------------------------------------------------------------------------
var
  PrevDllProc:  Pointer;
  DLLParam:     PtrInt;

//------------------------------------------------------------------------------
{
  Reserved should be a pointer, but meh - this thing is one giant mess in
  existing compilers...

    In old Delphi, Reserved is Integer. In new ones, it is Pointer.

    FPC has global variable DLLParam, which holds value of Reserved. And it is
    declared as PtrInt.

    bla bla bla... >:/

  This should work everywhere, if the compiler (and RTL code) is sane. If not,
  please let me know.
}
procedure ProcessGlobalVarsDllProc(Reason: Integer; Reserved: PtrInt);
type
  TLocalDLLProc = procedure(Reason: Integer; Reserved: PtrInt);
begin
DLLParam := Reserved;
If Assigned(PrevDllProc) then
  TLocalDLLProc(PrevDllProc)(Reason,Reserved);
end;
{$ENDIF}

//------------------------------------------------------------------------------

procedure ModuleInitialization;
var
  Modules:      TModuleArray;
  i:            Integer;
  GetHeadFunc:  Pointer;
  HeadTemp:     PPGVHead;
begin
{
  NOTE - this function is called when a module is loaded - this loading is
         serialized by the OS, so there is no need for synchronization.
}
{$IFNDEF FPC}
{
  Setup dllmain hook to intercept lpvReserved parameter when dll is detached
  (see ModuleFinalization for details).

  Also note that the hook is called BEFORE finalization of all units, including
  this one (at least that is the documented behaviour).
}
PrevDllProc := @SysInit.DllProcEx;
SysInit.DllProcEx := TDLLProcEx(@ProcessGlobalVarsDllProc);
{$ENDIF}
// enumerate modules loaded within this process...
Modules := EnumerateProcessModules;
{
  Traverse all loaded modules and look whether they export a properly named
  function.

  If this function is found, call it - when it returns nil, it means it is
  incompatible with this implementation and continue searching.
  When it returns non-nil pointer, use that for head pointer global variable,
  increment its reference counter and exit.

  If no function of that name is found or all exported functions return nil, do
  full initialization (allocation) here.

  Note that the modules contain the current module - this is not a problem since
  calling local *GetHead function will just return nil (as the global variable
  is not yet initilized) and therefore it will be ignored.
}
For i := Low(Modules) to High(Modules) do
  begin
    GetHeadFunc := GetProcAddress(Modules[i],PGV_EXPORTNAME_GETHEAD);
    If Assigned(GetHeadFunc) then
      begin
        HeadTemp := TGetHeadFunc(GetHeadFunc)(PGV_VERSION_CURRENT);
        If Assigned(HeadTemp) then
          begin
            Inc(HeadTemp^.RefCount);
            VAR_HeadPtr := HeadTemp;
            // we have everything we need, do not continue this function
            Exit;
          end;
      end;
  end;
{
  If we are here it means no currently loaded module provides usable PGV, we
  need to allocete it ourselves.
}
VAR_HeadPtr := GlobalMemoryAllocate(SizeOf(TPGVHead));
// initialize head fields (fields not explicitly touched here are zeroed)
VAR_HeadPtr^.RefCount := 1;
InitializeCriticalSection(VAR_HeadPtr^.Lock);
end;

{$ELSE}//=======================================================================
type
  dl_phdr_info = record
    dlpi_addr:      PtrUInt;
    dlpi_name:      PChar;
    dlpi_phdr:      Pointer;  // structure, but we do not need it
    dlpi_phnum:     UInt16;
    dlpi_adds:      cuLongLong;
    dlpi_subs:      cuLongLong;
    dlpi_tls_modid: size_t;
    dlpi_tls_data:  Pointer;
  end;
  dl_phdr_info_p = ^dl_phdr_info;

  TDLIterCallback = Function(info: dl_phdr_info_p; size: size_t; data: Pointer): cInt; cdecl;

Function dl_iterate_phdr(callback: TDLIterCallback; data: Pointer): cInt; cdecl; external;

type
  TModulesArray = record
    Arr:    array of String;
    Count:  Integer;
  end;
  PModulesArray = ^TModulesArray;

//------------------------------------------------------------------------------

Function ModuleEnumCallback(info: dl_phdr_info_p; size: size_t; data: Pointer): cInt; cdecl;
begin
If Assigned(info) and (size >= (2 * SizeOf(Pointer))) then
  begin
    If PModulesArray(Data)^.Count <= Length(PModulesArray(Data)^.Arr) then
      SetLength(PModulesArray(Data)^.Arr,Length(PModulesArray(Data)^.Arr) + 16);
    PModulesArray(Data)^.Arr[PModulesArray(Data)^.Count] := String(info^.dlpi_name);
    Inc(PModulesArray(Data)^.Count);
  end;
Result := 0;
end;

//------------------------------------------------------------------------------

procedure EnumerateProcessModules(out Modules: TModulesArray);
begin
Modules.Arr := nil;
Modules.Count := 0;
dl_iterate_phdr(@ModuleEnumCallback,@Modules);
SetLength(Modules.Arr,Modules.Count);
end;

//------------------------------------------------------------------------------

procedure ModuleInitialization;
var
  Modules:      TModulesArray;
  i:            Integer;
  ProbedMod:    Pointer;
  GetHeadFunc:  Pointer;
  HeadTemp:     PPGVHead;
  LocalHead:    TPGVHead;
begin
// enumerate modules
EnumerateProcessModules(Modules);
// traverse modules to find the one containing gethead function
For i := Low(Modules.Arr) to Pred(Modules.Count) do
  begin
    ProbedMod := dlopen(PChar(Modules.Arr[i]),RTLD_NOW);
    If Assigned(ProbedMod) then
      try
        GetHeadFunc := dlsym(ProbedMod,PGV_EXPORTNAME_GETHEAD);
        If Assigned(GetHeadFunc) then
          begin
            HeadTemp := TGetHeadFunc(GetHeadFunc)(PGV_VERSION_CURRENT);
            If Assigned(HeadTemp) then
              begin
                Inc(HeadTemp^.RefCount);
                VAR_HeadPtr := HeadTemp;
                Exit;
              end;
          end;
      finally
        If dlclose(ProbedMod) <> 0 then
          raise EPGVSystemError.CreateFmt('ModuleInitialization: Failed to close probed module (%s).',[dlerror]);
      end
    else raise EPGVSystemError.CreateFmt('ModuleInitialization: Failed to open probed module (%s).',[dlerror]);
  end;
{
  No module with prepared head found.

  The head is allocated using global memory allocation routines, but these
  require that the head is already prepared - catch 22. We use local variable
  to provide what the allocation rutines require.
}
LocalHead.Allocator.AllocFunc := @lin_malloc;
VAR_HeadPtr := @LocalHead;
VAR_HeadPtr := GlobalMemoryAllocate(SizeOf(TPGVHead));
VAR_HeadPtr^.RefCount := 1;
GlobalMemoryInit;
ThreadLockInit;
end;

{$ENDIF}

{===============================================================================
    PGV internal implementation - module finalization
===============================================================================}

procedure FreeAllVariablesAndSegments;
var
  CurrentSegment: PPGVSegment;
  NextSegment:    PPGVSegment;
  i:              Integer;
begin
{
  We are last module using current VAR_HeadPtr, there should be no other threads
  able to access it by this point, but we still enter critical section to be
  sure. But, since this call is serialized by OS, we cannot block - that could
  create deadlock - instead non blocking try-enter is used here.
}
If TryThreadLock then
  try
    CurrentSegment := VAR_HeadPtr^.FirstSegment;
    VAR_HeadPtr^.FirstSegment := nil;
    while Assigned(CurrentSegment) do
      begin
      {
        Free individual entries (variables) - note there is no need to clear
        individual entries as the entire segment will be freed.
      }
        For i := Low(CurrentSegment^.Entries) to High(CurrentSegment^.Entries) do
          If ((CurrentSegment^.Entries[i].Flags and PGV_SEFLAG_USED) <> 0) and
             ((CurrentSegment^.Entries[i].Flags and PGV_SEFLAG_SMLSIZE_MASK) = 0) then
            GlobalMemoryFree(CurrentSegment^.Entries[i].Address);
        // free the segment
        NextSegment := CurrentSegment^.Head.NextSegment;
        GlobalMemoryFree(Pointer(CurrentSegment));
        CurrentSegment := NextSegment;
      end;
  finally
    ThreadUnlock;
  end
else raise EPGVModuleCleanupError.Create('FreeAllVariablesAndSegments: Unable to acquire lock.');
end;

//------------------------------------------------------------------------------
{$IFDEF Windows}

procedure ModuleFinalization;
begin
{
  NOTE - this function is called when a module is unloaded, which is serialized
  on a process-wide basis by the OS, so there is no need for synchronization.

  Only call cleanup when a library is explicitly unloaded (parameter Reserved
  (here stored in DLLParam) in DLLMain is null/nil), do not call it when the
  process is terminating - freeing the heap then would cause an error
  (DbgBreakPoint is called by kernel).
}
If Assigned(VAR_HeadPtr) and (DLLParam = 0) then
  begin
    Dec(VAR_HeadPtr^.RefCount);
    If VAR_HeadPtr^.RefCount <= 0 then
      begin
        FreeAllVariablesAndSegments;
        DeleteCriticalSection(VAR_HeadPtr^.Lock);
        VAR_HeadPtr^.RefCount := 0;
        GlobalMemoryFree(Pointer(VAR_HeadPtr));
        VAR_HeadPtr := nil;
      end;
  end;
end;

{$ELSE}//=======================================================================

procedure ModuleFinalization;
var
  LocalHead:  TPGVHead;
begin
If Assigned(VAR_HeadPtr) then
  begin
    Dec(VAR_HeadPtr^.RefCount);
    If VAR_HeadPtr^.RefCount <= 0 then
      begin
        FreeAllVariablesAndSegments;
        ThreadLockFinal;
        VAR_HeadPtr^.RefCount := 0;
        LocalHead := VAR_HeadPtr^;
        GlobalMemoryFree(Pointer(VAR_HeadPtr));
        VAR_HeadPtr := @LocalHead;
        GlobalMemoryFinal;
        VAR_HeadPtr := nil;
      end;
  end;
end;

{$ENDIF}


{===============================================================================
--------------------------------------------------------------------------------
                       PGV public interface implementation
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    PGV public interface implementation - main implementation
===============================================================================}

Function GlobVarTranslateIdentifier(const Identifier: String): TPGVIdentifier;
begin
Result := TPGVIdentifier(WideStringAdler32(StrToWide(Identifier)));
end;

//==============================================================================

procedure GlobVarLock;
begin
ThreadLock;
end;

//------------------------------------------------------------------------------

procedure GlobVarUnlock;
begin
ThreadUnlock
end;

//------------------------------------------------------------------------------

Function GlobVarCount: Integer;
var
  CurrentSegment: PPGVSegment;
begin
GlobVarLock;
try
  Result := 0;
  CurrentSegment := VAR_HeadPtr^.FirstSegment;
  while Assigned(CurrentSegment) do
    begin
      Inc(Result,CurrentSegment^.Head.AllocCount);
      CurrentSegment := CurrentSegment^.Head.NextSegment;
    end;
finally
  GlobVarUnlock;
end;
end;

//------------------------------------------------------------------------------

Function GlobVarMemory(IncludeOverhead: Boolean = False): TMemSize;
var
  CurrentSegment: PPGVSegment;
  i:              Integer;
begin
GlobVarLock;
try
  If IncludeOverhead then
    Result := SizeOf(TPGVHead)
  else
    Result := 0;
  CurrentSegment := VAR_HeadPtr^.FirstSegment;
  while Assigned(CurrentSegment) do
    begin
      If IncludeOverhead then
        begin
          Inc(Result,SizeOf(TPGVSegment));
          If CurrentSegment^.Head.AllocCount > 0 then
            For i := Low(CurrentSegment^.Entries) to High(CurrentSegment^.Entries) do
              If (CurrentSegment^.Entries[i].Flags and PGV_SEFLAG_USED) <> 0 then
                // count only entries that are allocated on heap, ignore those stored in-situ
                If (CurrentSegment^.Entries[i].Flags and PGV_SEFLAG_SMLSIZE_MASK) = 0 then
                  Inc(Result,CurrentSegment^.Entries[i].Size);
        end
      else
        begin
          If CurrentSegment^.Head.AllocCount > 0 then
            For i := Low(CurrentSegment^.Entries) to High(CurrentSegment^.Entries) do
              If (CurrentSegment^.Entries[i].Flags and PGV_SEFLAG_USED) <> 0 then
                Inc(Result,EntrySize(Addr(CurrentSegment^.Entries[i])));
        end;
      CurrentSegment := CurrentSegment^.Head.NextSegment;
    end;
finally
  GlobVarUnlock;
end;
end;

//------------------------------------------------------------------------------

Function GlobVarEnumerate: TPGVIdentifierArray;
var
  ResultIndex:    Integer;
  CurrentSegment: PPGVSegment;
  i:              Integer;
begin
Result := nil;
GlobVarLock;
try
  SetLength(Result,GlobVarCount);
  If Length(Result) > 0 then
    begin
      ResultIndex := Low(Result);
      CurrentSegment := VAR_HeadPtr^.FirstSegment;
      while Assigned(CurrentSegment) do
        begin
          If CurrentSegment^.Head.AllocCount > 0 then
            For i := Low(CurrentSegment^.Entries) to High(CurrentSegment^.Entries) do
              If (CurrentSegment^.Entries[i].Flags and PGV_SEFLAG_USED) <> 0 then
                begin
                  Result[ResultIndex] := CurrentSegment^.Entries[i].Identifier;
                  Inc(ResultIndex);
                end;
          CurrentSegment := CurrentSegment^.Head.NextSegment;
        end;
    end;
finally
  GlobVarUnlock;
end;
end;

//==============================================================================

Function GlobVarExists(Identifier: TPGVIdentifier): Boolean;
var
  Segment:  PPGVSegment;
  Entry:    PPGVSegmentEntry;
begin
GlobVarLock;
try
  Result := EntryFind(Identifier,Segment,Entry);
finally
  GlobVarUnlock;
end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function GlobVarExists(const Identifier: String): Boolean;
begin
Result := GlobVarExists(GlobVarTranslateIdentifier(Identifier));
end;

//------------------------------------------------------------------------------

Function GlobVarFind(Identifier: TPGVIdentifier; out Variable: TPGVVariable): Boolean;
var
  Segment:  PPGVSegment;
  Entry:    PPGVSegmentEntry;
begin
GlobVarLock;
try
  If EntryFind(Identifier,Segment,Entry) then
    Variable := Addr(Entry^.Address)
  else
    Variable := nil;
  Result := Assigned(Variable);
finally
  GlobVarUnlock;
end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function GlobVarFind(const Identifier: String; out Variable: TPGVVariable): Boolean;
begin
Result := GlobVarFind(GlobVarTranslateIdentifier(Identifier),Variable);
end;

//------------------------------------------------------------------------------

Function GlobVarGet(Identifier: TPGVIdentifier): TPGVVariable;
var
  Segment:  PPGVSegment;
  Entry:    PPGVSegmentEntry;
begin
Result := nil;
GlobVarLock;
try
  If EntryFind(Identifier,Segment,Entry) then
    Result := Addr(Entry^.Address)
  else
    raise EPGVUnknownVariable.CreateFmt('GlobVarGet: Unknown variable 0x%.8x.',[Identifier]);
finally
  GlobVarUnlock;
end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function GlobVarGet(const Identifier: String): TPGVVariable;
var
  Segment:  PPGVSegment;
  Entry:    PPGVSegmentEntry;
begin
Result := nil;
GlobVarLock;
try
  If EntryFind(GlobVarTranslateIdentifier(Identifier),Segment,Entry) then
    Result := Addr(Entry^.Address)
  else
    raise EPGVUnknownVariable.CreateFmt('GlobVarGet: Unknown variable "%s".',[Identifier]);
finally
  GlobVarUnlock;
end;
end;

//==============================================================================

Function GlobVarSize(Identifier: TPGVIdentifier): TMemSize;
var
  Segment:  PPGVSegment;
  Entry:    PPGVSegmentEntry;
begin
Result := 0;
GlobVarLock;
try
  If EntryFind(Identifier,Segment,Entry) then
    Result := EntrySize(Entry)
  else
    raise EPGVUnknownVariable.CreateFmt('GlobVarSize: Unknown variable 0x%.8x.',[Identifier]);
finally
  GlobVarUnlock;
end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function GlobVarSize(const Identifier: String): TMemSize;
var
  Segment:  PPGVSegment;
  Entry:    PPGVSegmentEntry;
begin
Result := 0;
GlobVarLock;
try
  If EntryFind(GlobVarTranslateIdentifier(Identifier),Segment,Entry) then
    Result := EntrySize(Entry)
  else
    raise EPGVUnknownVariable.CreateFmt('GlobVarSize: Unknown variable "%s".',[Identifier]);
finally
  GlobVarUnlock;
end;
end;

//------------------------------------------------------------------------------

Function GlobVarHeapStored(Identifier: TPGVIdentifier): Boolean;
var
  Segment:  PPGVSegment;
  Entry:    PPGVSegmentEntry;
begin
Result := False;
GlobVarLock;
try
  If EntryFind(Identifier,Segment,Entry) then
    Result := (Entry^.Flags and PGV_SEFLAG_SMLSIZE_MASK) = 0
  else
    raise EPGVUnknownVariable.CreateFmt('GlobVarHeapStored: Unknown variable 0x%.8x.',[Identifier]);
finally
  GlobVarUnlock;
end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function GlobVarHeapStored(const Identifier: String): Boolean;
var
  Segment:  PPGVSegment;
  Entry:    PPGVSegmentEntry;
begin
Result := False;
GlobVarLock;
try
  If EntryFind(GlobVarTranslateIdentifier(Identifier),Segment,Entry) then
    Result := (Entry^.Flags and PGV_SEFLAG_SMLSIZE_MASK) = 0
  else
    raise EPGVUnknownVariable.CreateFmt('GlobVarHeapStored: Unknown variable "%s".',[Identifier]);
finally
  GlobVarUnlock;
end;
end;

//==============================================================================

Function GlobVarAllocate(Identifier: TPGVIdentifier; Size: TMemSize): TPGVVariable;
var
  Segment:  PPGVSegment;
  Entry:    PPGVSegmentEntry;
begin
Result := nil;
GlobVarLock;
try
  If not EntryFind(Identifier,Segment,Entry) then
    begin
      If Size > 0 then
        begin
          EntryAllocate(Identifier,Size,Segment,Entry);
          Result := Addr(Entry^.Address);
        end
      else Result := nil;
    end
  else raise EPGVDuplicateVariable.CreateFmt('GlobVarAllocate: Variable 0x%.8x already exists.',[Identifier]);
finally
  GlobVarUnlock;
end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function GlobVarAllocate(const Identifier: String; Size: TMemSize): TPGVVariable;
var
  Segment:  PPGVSegment;
  Entry:    PPGVSegmentEntry;
begin
Result := nil;
GlobVarLock;
try
  If not EntryFind(GlobVarTranslateIdentifier(Identifier),Segment,Entry) then
    begin
      If Size > 0 then
        begin
          EntryAllocate(GlobVarTranslateIdentifier(Identifier),Size,Segment,Entry);
          Result := Addr(Entry^.Address);
        end
      else Result := nil;
    end
  else raise EPGVDuplicateVariable.CreateFmt('GlobVarAllocate: Variable "%s" already exists.',[Identifier]);
finally
  GlobVarUnlock;
end;
end;

//------------------------------------------------------------------------------

Function GlobVarAlloc(Identifier: TPGVIdentifier; Size: TMemSize): TPGVVariable;
begin
Result := GlobVarAllocate(Identifier,Size);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function GlobVarAlloc(const Identifier: String; Size: TMemSize): TPGVVariable;
begin
Result := GlobVarAllocate(Identifier,Size);
end;

//------------------------------------------------------------------------------

Function GlobVarReallocate(Identifier: TPGVIdentifier; NewSize: TMemSize): TPGVVariable;
var
  Segment:  PPGVSegment;
  Entry:    PPGVSegmentEntry;
begin
Result := nil;
GlobVarLock;
try
  If EntryFind(Identifier,Segment,Entry) then
    begin
      // entry already exists (its size cannot be zero)
      If NewSize > 0 then
        begin
          EntryReallocate(Segment,Entry,NewSize);
          Result := Addr(Entry^.Address);
        end
      else EntryFree(Segment,Entry);
    end
  // requested entry does not exist
  else If NewSize > 0 then
    begin
      EntryAllocate(Identifier,NewSize,Segment,Entry);
      Result := Addr(Entry^.Address);
    end;
finally
  GlobVarUnlock;
end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function GlobVarReallocate(const Identifier: String; NewSize: TMemSize): TPGVVariable;
begin
Result := GlobVarReallocate(GlobVarTranslateIdentifier(Identifier),NewSize);
end;

//------------------------------------------------------------------------------

Function GlobVarRealloc(Identifier: TPGVIdentifier; NewSize: TMemSize): TPGVVariable;
begin
Result := GlobVarReallocate(Identifier,NewSize);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function GlobVarRealloc(const Identifier: String; NewSize: TMemSize): TPGVVariable;
begin
Result := GlobVarReallocate(Identifier,NewSize);
end;

//------------------------------------------------------------------------------

procedure GlobVarFree(Identifier: TPGVIdentifier);
var
  Segment:  PPGVSegment;
  Entry:    PPGVSegmentEntry;
begin
GlobVarLock;
try
  If EntryFind(Identifier,Segment,Entry) then
    EntryFree(Segment,Entry)
  else
    raise EPGVUnknownVariable.CreateFmt('GlobVarFree: Unknown variable 0x%.8x.',[Identifier]);
finally
  GlobVarUnlock;
end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure GlobVarFree(const Identifier: String);
var
  Segment:  PPGVSegment;
  Entry:    PPGVSegmentEntry;
begin
GlobVarLock;
try
  If EntryFind(GlobVarTranslateIdentifier(Identifier),Segment,Entry) then
    EntryFree(Segment,Entry)
  else
    raise EPGVUnknownVariable.CreateFmt('GlobVarFree: Unknown variable "%s".',[Identifier]);
finally
  GlobVarUnlock;
end;
end;

//==============================================================================

Function GlobVarStore(Identifier: TPGVIdentifier; const Buffer; Count: TMemSize): TMemSize;
var
  Segment:  PPGVSegment;
  Entry:    PPGVSegmentEntry;
begin
Result := 0;
GlobVarLock;
try
  If EntryFind(Identifier,Segment,Entry) then
    begin
      Result := Min(Count,EntrySize(Entry));
      Move(Buffer,Entry^.Address^,Result);
    end
  else raise EPGVUnknownVariable.CreateFmt('GlobVarStore: Unknown variable 0x%.8x.',[Identifier]);
finally
  GlobVarUnlock;
end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function GlobVarStore(const Identifier: String; const Buffer; Count: TMemSize): TMemSize;
var
  Segment:  PPGVSegment;
  Entry:    PPGVSegmentEntry;
begin
Result := 0;
GlobVarLock;
try
  If EntryFind(GlobVarTranslateIdentifier(Identifier),Segment,Entry) then
    begin
      Result := Min(Count,EntrySize(Entry));
      Move(Buffer,Entry^.Address^,Result);
    end
  else raise EPGVUnknownVariable.CreateFmt('GlobVarStore: Unknown variable "%s".',[Identifier]);
finally
  GlobVarUnlock;
end;
end;

//------------------------------------------------------------------------------

Function GlobVarLoad(Identifier: TPGVIdentifier; out Buffer; Count: TMemSize): TMemSize;
var
  Segment:  PPGVSegment;
  Entry:    PPGVSegmentEntry;
begin
Result := 0;
GlobVarLock;
try
  If EntryFind(Identifier,Segment,Entry) then
    begin
      Result := Min(Count,EntrySize(Entry));
      Move(Entry^.Address^,Addr(Buffer)^,Result);
    end
  else raise EPGVUnknownVariable.CreateFmt('GlobVarLoad: Unknown variable 0x%.8x.',[Identifier]);
finally
  GlobVarUnlock;
end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function GlobVarLoad(const Identifier: String; out Buffer; Count: TMemSize): TMemSize;
var
  Segment:  PPGVSegment;
  Entry:    PPGVSegmentEntry;
begin
Result := 0;
GlobVarLock;
try
  If EntryFind(GlobVarTranslateIdentifier(Identifier),Segment,Entry) then
    begin
      Result := Min(Count,EntrySize(Entry));
      Move(Entry^.Address^,Addr(Buffer)^,Result);
    end
  else raise EPGVUnknownVariable.CreateFmt('GlobVarLoad: Unknown variable "%s".',[Identifier]);
finally
  GlobVarUnlock;
end;
end;

//------------------------------------------------------------------------------

Function GlobVarTryGet(Identifier: TPGVIdentifier; out Variable: TPGVVariable; out Size: TMemSize): Boolean;
var
  Segment:  PPGVSegment;
  Entry:    PPGVSegmentEntry;
begin
Result := False;
Variable := nil;
Size := 0;
GlobVarLock;
try
  If EntryFind(Identifier,Segment,Entry) then
    begin
      Variable := Addr(Entry^.Address);
      Size := EntrySize(Entry);
      Result := True;
    end;
finally
  GlobVarUnlock;
end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function GlobVarTryGet(const Identifier: String; out Variable: TPGVVariable; out Size: TMemSize): Boolean;
begin
Result := GlobVarTryGet(GlobVarTranslateIdentifier(Identifier),Variable,Size);
end;


{===============================================================================
--------------------------------------------------------------------------------
                  Unit exports, initialization and finalization                  
--------------------------------------------------------------------------------
===============================================================================}
exports
  ProcessGlobalVarsGetHead name PGV_EXPORTNAME_GETHEAD;

initialization
  ModuleInitialization;

finalization
  ModuleFinalization;

end.

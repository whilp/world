/*-*- mode:c;indent-tabs-mode:nil;c-basic-offset:2;tab-width:8;coding:utf-8 -*-│
│ vi: set et ft=c ts=2 sts=2 sw=2 fenc=utf-8                               :vi │
╞══════════════════════════════════════════════════════════════════════════════╡
│ Copyright 2022 Justine Alexandra Roberts Tunney                              │
│ Copyright 2024 Will Maier                                                    │
│                                                                              │
│ Permission to use, copy, modify, and/or distribute this software for         │
│ any purpose with or without fee is hereby granted, provided that the         │
│ above copyright notice and this permission notice appear in all copies.      │
│                                                                              │
│ THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL                │
│ WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED                │
│ WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE             │
│ AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL         │
│ DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR        │
│ PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER               │
│ TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR             │
│ PERFORMANCE OF THIS SOFTWARE.                                                │
╚─────────────────────────────────────────────────────────────────────────────*/
/*
 * gVisor-compatible TLS initialization
 *
 * gVisor's Linux syscall emulation doesn't support ARCH_SET_GS, only ARCH_SET_FS.
 * This override detects gVisor at runtime by trying ARCH_SET_GS first; if it
 * fails with EINVAL, we fall back to ARCH_SET_FS and morph the TLS opcodes.
 *
 * See: https://gvisor.dev/docs/user_guide/compatibility/
 */
#include "libc/assert.h"
#include "libc/calls/calls.h"
#include "libc/calls/syscall-sysv.internal.h"
#include "libc/dce.h"
#include "libc/nexgen32e/msr.internal.h"
#include "libc/nt/thread.h"
#include "libc/sysv/consts/arch.h"
#include "libc/thread/tls.h"

#define AMD64_SET_FSBASE 129
#define AMD64_SET_GSBASE 131

int sys_set_tls(uintptr_t, void *);
void __morph_tls(void);

char __tls_using_fs;

dontinstrument textstartup void __set_tls(struct CosmoTib *tib) {
  tib = __adj_tls(tib);
#ifdef __x86_64__
  if (IsWindows()) {
    __set_tls_win32(tib);
  } else if (IsLinux()) {
    long rc = sys_set_tls(ARCH_SET_GS, tib);
    if (rc == -22) {
      sys_set_tls(ARCH_SET_FS, tib);
      __tls_using_fs = 1;
      __morph_tls();
    }
  } else if (IsFreebsd()) {
    sys_set_tls(AMD64_SET_GSBASE, tib);
  } else if (IsNetbsd()) {
    sys_set_tls((uintptr_t)tib, 0);
  } else if (IsOpenbsd()) {
    sys_set_tls((uintptr_t)tib, 0);
  } else if (IsXnu()) {
    sys_set_tls((intptr_t)tib - 0x30, 0);
  } else {
    uint64_t val = (uint64_t)tib;
    asm volatile("wrmsr"
                 : /* no outputs */
                 : "c"(MSR_IA32_GS_BASE), "a"((uint32_t)val),
                   "d"((uint32_t)(val >> 32)));
  }
#elif defined(__aarch64__)
  register long x28 asm("x28") = (long)tib;
  asm volatile("" : "+r"(x28));
#else
#error "unsupported architecture"
#endif
}

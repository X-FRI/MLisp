// Copyright (c) 2023 Muqiu Han
//
// All rights reserved.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are met:
//
//     * Redistributions of source code must retain the above copyright notice,
//       this list of conditions and the following disclaimer.
//     * Redistributions in binary form must reproduce the above copyright
//     notice,
//       this list of conditions and the following disclaimer in the
//       documentation and/or other materials provided with the distribution.
//     * Neither the name of Terifo nor the names of its contributors
//       may be used to endorse or promote products derived from this software
//       without specific prior written permission.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
// "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
// LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
// A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
// CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
// EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
// PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
// PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
// LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
// NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
// SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

#include "binop.hpp"
#include "panic/panic.hpp"

namespace swallow::compiler::gmachine
{
  [[nodiscard]] auto Binop::operatorsToString(utils::Binop op) noexcept
    -> std::string
  {
    switch (op)
      {
      case utils::Binop::PLUS:
        return {"+"};
      case utils::Binop::MINUS:
        return {"-"};
      case utils::Binop::TIMES:
        return {"*"};
      case utils::Binop::DIVIDE:
        return {"/"};
      }

    utils::panic("operatorsToString failed!!!");
  }

  [[nodiscard]] auto Binop::operatorsAction(utils::Binop op) noexcept
    -> std::string
  {
    switch (op)
      {
      case utils::Binop::PLUS:
        return {"plus"};
      case utils::Binop::MINUS:
        return {"minus"};
      case utils::Binop::TIMES:
        return {"times"};
      case utils::Binop::DIVIDE:
        return {"divede"};
      }

    utils::panic("operatorsAction failed!!!");
  }
} // namespace swallow::compiler::gmachine
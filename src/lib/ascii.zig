//! Constants for the POSIX/C locale & charset.
//!
//! See POSIX.1-2024, Sections 6.1, 6.4, 7.2.

const ascii = @This();

pub const Class = enum {
    alnum,
    alpha,
    blank,
    cntrl,
    digit,
    graph,
    lower,
    print,
    punct,
    space,
    upper,
    xdigit,

    pub fn mask(class: Class) u128 {
        @setEvalBranchQuota(2000);
        return switch (class) {
            inline else => |c| condToMask(c.cond()),
        };
    }

    pub fn contains(class: Class, ch: u7) bool {
        return switch (class) {
            inline else => |c| c.mask() & @as(u128, 1) << ch != 0,
        };
    }

    pub fn cond(comptime class: Class) fn (u7) bool {
        return switch (class) {
            .alnum => isAlnum,
            .alpha => isAlpha,
            .blank => isBlank,
            .cntrl => isCntrl,
            .digit => isDigit,
            .graph => isGraph,
            .lower => isLower,
            .print => isPrint,
            .punct => isPunct,
            .space => isSpace,
            .upper => isUpper,
            .xdigit => isXdigit,
        };
    }

    fn isAlnum(ch: u7) bool {
        return isAlpha(ch) or isDigit(ch);
    }

    fn isAlpha(ch: u7) bool {
        return isUpper(ch) or isLower(ch);
    }

    fn isBlank(ch: u7) bool {
        return ch == ' ' or ch == '\t';
    }

    fn isCntrl(ch: u7) bool {
        return switch (ch) {
            0x00...0x1F => true,
            else => false,
        };
    }

    fn isDigit(ch: u7) bool {
        return switch (ch) {
            '0'...'9' => true,
            else => false,
        };
    }

    fn isGraph(ch: u7) bool {
        return isPrint(ch) and ch != ' ';
    }

    fn isLower(ch: u7) bool {
        return switch (ch) {
            'a'...'z' => true,
            else => false,
        };
    }

    fn isPrint(ch: u7) bool {
        return !isCntrl(ch);
    }

    fn isPunct(ch: u7) bool {
        return isGraph(ch) and !isAlnum(ch);
    }

    fn isSpace(ch: u7) bool {
        return switch (ch) {
            0x09...0x0D, ' ' => true,
            else => false,
        };
    }

    fn isUpper(ch: u7) bool {
        return switch (ch) {
            'A'...'Z' => true,
            else => false,
        };
    }

    fn isXdigit(ch: u7) bool {
        return switch (ch) {
            '0'...'9', 'A'...'F', 'a'...'f' => true,
            else => false,
        };
    }
};

pub fn condToMask(comptime condition: fn (u7) bool) u128 {
    comptime var result: u128 = 0;
    comptime {
        for (0..128) |ch_usize| {
            const ch: u7 = @intCast(ch_usize);
            if (condition(ch)) {
                result |= @as(u128, 1) << ch;
            }
        }
    }
    return result;
}

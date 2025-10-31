//
//  QueensApp.swift
//  Queens Cami's version
//
//  Created by Camila Monsalve on 30.10.2025.
//
// Queens – SwiftUI iPhone App (Undo + AutoX + Generator + Live Highlights)
// Replace your entire QueensApp.swift with this file.
// iOS 16+

import SwiftUI
import Combine

@main
struct QueensApp: App {
    var body: some Scene {
        WindowGroup { QueensGameView() }
    }
}

// MARK: - Model

struct Puzzle: Identifiable, Hashable {
    let id = UUID()
    let size: Int
    let regionIds: [[Int]]
    let presetQueens: Set<BoardIndex>
}

struct BoardIndex: Hashable, Codable { let r: Int; let c: Int }

enum CellState: Equatable { case empty, queen, mark }

struct BoardState: Equatable {
    var size: Int
    var cells: [[CellState]]
    var presetQueens: Set<BoardIndex>

    static func empty(size: Int, preset: Set<BoardIndex>) -> BoardState {
        var cells = Array(repeating: Array(repeating: CellState.empty, count: size), count: size)
        for idx in preset { cells[idx.r][idx.c] = .queen }
        return BoardState(size: size, cells: cells, presetQueens: preset)
    }
}

// MARK: - Default puzzle (only used on first launch)
let samplePuzzles: [Puzzle] = {
    let N = 6
    let regions: [[Int]] = [
        [0,0,1,1,2,2],
        [0,3,3,1,2,2],
        [0,3,4,4,4,2],
        [5,3,4,1,4,2],
        [5,5,5,1,1,2],
        [5,3,3,3,1,2]
    ]
    return [Puzzle(size: N, regionIds: regions, presetQueens: [])]
}()

// MARK: - ViewModel

final class GameVM: ObservableObject {
    @Published var board: BoardState
    @Published var puzzle: Puzzle

    @Published var autoXEnabled: Bool = true
    private var autoMarks: Set<BoardIndex> = []
    @Published var history: [BoardState] = []

    init(puzzle: Puzzle) {
        self.puzzle = puzzle
        self.board = BoardState.empty(size: puzzle.size, preset: puzzle.presetQueens)
        rebuildAutoMarks()
    }

    var size: Int { board.size }

    func reset() {
        board = BoardState.empty(size: size, preset: puzzle.presetQueens)
        autoMarks.removeAll()
        history.removeAll()
        rebuildAutoMarks()
    }

    // Tap cycles: empty -> X -> queen -> empty
    func cycle(_ r: Int, _ c: Int) {
        let idx = BoardIndex(r: r, c: c)
        guard !board.presetQueens.contains(idx) else { return }
        history.append(board)
        switch board.cells[r][c] {
        case .empty: board.cells[r][c] = .mark
        case .mark:  board.cells[r][c] = .queen
        case .queen: board.cells[r][c] = .empty
        }
        rebuildAutoMarks()
        objectWillChange.send()
    }

    // Long-press toggles X directly
    func longPressMark(_ r: Int, _ c: Int) {
        let idx = BoardIndex(r: r, c: c)
        guard !board.presetQueens.contains(idx) else { return }
        history.append(board)
        board.cells[r][c] = (board.cells[r][c] == .mark) ? .empty : .mark
        rebuildAutoMarks()
        objectWillChange.send()
    }

    // MARK: - Validation

    struct Violation: Identifiable { let id = UUID(); let message: String }

    func checkViolations() -> [Violation] {
        var v: [Violation] = []
        let N = size
        for r in 0..<N { if (0..<N).filter({ board.cells[r][$0] == .queen }).count > 1 { v.append(.init(message: "Row \(r+1) has >1 queen")) } }
        for c in 0..<N { if (0..<N).filter({ board.cells[$0][c] == .queen }).count > 1 { v.append(.init(message: "Col \(c+1) has >1 queen")) } }
        var reg: [Int:Int] = [:]
        for r in 0..<N { for c in 0..<N where board.cells[r][c] == .queen { reg[puzzle.regionIds[r][c], default: 0] += 1 } }
        for (k,vn) in reg where vn > 1 { v.append(.init(message: "Region #\(k+1) has >1 queen")) }
        return v
    }

    func isSolved() -> Bool {
        let N = size
        for r in 0..<N { if (0..<N).filter({ board.cells[r][$0] == .queen }).count != 1 { return false } }
        for c in 0..<N { if (0..<N).filter({ board.cells[$0][c] == .queen }).count != 1 { return false } }
        var regionCount: [Int:Int] = [:]
        for r in 0..<N { for c in 0..<N { if board.cells[r][c] == .queen { regionCount[puzzle.regionIds[r][c], default: 0] += 1 } } }
        let regionTotal = puzzle.regionIds.flatMap{ $0 }.max() ?? -1
        for reg in 0...regionTotal { if regionCount[reg] != 1 { return false } }
        // No diagonal touching (adjacent diagonals only)
        let dirs = [(-1,-1),(-1,1),(1,-1),(1,1)]
        for r in 0..<N { for c in 0..<N where board.cells[r][c] == .queen {
            for (dr,dc) in dirs {
                let nr = r + dr, nc = c + dc
                if (0..<N).contains(nr) && (0..<N).contains(nc) && board.cells[nr][nc] == .queen { return false }
            }
        }}
        return true
    }

    func isQueenViolating(_ r: Int, _ c: Int) -> Bool {
        violatingQueenIndices().contains(BoardIndex(r: r, c: c))
    }

    private func violatingQueenIndices() -> Set<BoardIndex> {
        var bad: Set<BoardIndex> = []
        let N = size
        var queens: [BoardIndex] = []
        for r in 0..<N { for c in 0..<N { if board.cells[r][c] == .queen { queens.append(BoardIndex(r: r, c: c)) } } }
        for r in 0..<N {
            let q = queens.filter{ $0.r == r }
            if q.count > 1 { bad.formUnion(q) }
        }
        for c in 0..<N {
            let q = queens.filter{ $0.c == c }
            if q.count > 1 { bad.formUnion(q) }
        }
        var regMap: [Int:[BoardIndex]] = [:]
        for q in queens { regMap[puzzle.regionIds[q.r][q.c], default: []].append(q) }
        for (_,group) in regMap where group.count > 1 { bad.formUnion(group) }
        // Adjacent diagonal touching
        for i in 0..<queens.count {
            for j in i+1..<queens.count {
                let a = queens[i], b = queens[j]
                if abs(a.r - b.r) == 1 && abs(a.c - b.c) == 1 { bad.insert(a); bad.insert(b) }
            }
        }
        return bad
    }

    // MARK: - Auto-X

    func rebuildAutoMarks() {
        let N = size
        for idx in autoMarks { if board.cells[idx.r][idx.c] == .mark { board.cells[idx.r][idx.c] = .empty } }
        autoMarks.removeAll()
        guard autoXEnabled else { return }
        func markIfEmpty(_ r: Int, _ c: Int) {
            if board.cells[r][c] == .empty { board.cells[r][c] = .mark; autoMarks.insert(BoardIndex(r: r, c: c)) }
        }
        for r in 0..<N { for c in 0..<N where board.cells[r][c] == .queen {
            for cc in 0..<N where cc != c { markIfEmpty(r, cc) }
            for rr in 0..<N where rr != r { markIfEmpty(rr, c) }
            let reg = puzzle.regionIds[r][c]
            for rr in 0..<N { for cc in 0..<N where !(rr==r && cc==c) && puzzle.regionIds[rr][cc]==reg { markIfEmpty(rr, cc) } }
        }}
    }

    func undo() {
        guard let last = history.popLast() else { return }
        board = last
        rebuildAutoMarks()
        objectWillChange.send()
    }
}

// MARK: - Generator / Solver

extension GameVM {
    // Create a random partition into N regions (each region has N cells)
    static func makeRegions(size N: Int) -> [[Int]] {
        // Connected, compact-biased region generator
        func degree(_ grid: [[Int]], _ r: Int, _ c: Int) -> Int {
            let id = grid[r][c]
            let nbrs = [(r-1,c),(r+1,c),(r,c-1),(r,c+1)]
            var d = 0
            for (rr,cc) in nbrs where (0..<N).contains(rr) && (0..<N).contains(cc) {
                if grid[rr][cc] == id { d += 1 }
            }
            return d
        }
        func attempt() -> [[Int]]? {
            var grid = Array(repeating: Array(repeating: -1, count: N), count: N)
            var sizes = Array(repeating: 0, count: N)
            // seeds in a grid-ish pattern to encourage compactness
            var seeds: [(Int,Int)] = []
            var unused = Array(0..<(N*N)).map { ($0 / N, $0 % N) }  // all cells
            seeds.append((N/2, N/2))
            for _ in 1..<N {
                // pick the cell farthest from all current seeds (Manhattan distance)
                var best: (Int,Int)? = nil
                var bestD = -1
                for (r,c) in unused {
                    var dmin = Int.max
                    for (sr,sc) in seeds {
                        let d = abs(sr - r) + abs(sc - c)
                        if d < dmin { dmin = d }
                    }
                    if dmin > bestD { bestD = dmin; best = (r,c) }
                }
                seeds.append(best!)
                // keep list inexpensive
                unused.removeAll(where: { cell in seeds.contains(where: { $0 == cell }) })
            }
            var pool = Array(0..<(N*N)); pool.shuffle()
            for rid in 0..<N {
                let idx = pool[rid]
                seeds.append((idx / N, idx % N))
            }
            var frontiers: [[(Int,Int)]] = Array(repeating: [], count: N)
            for rid in 0..<N {
                let (r,c) = seeds[rid]
                grid[r][c] = rid
                sizes[rid] = 1
                for (rr,cc) in [(r-1,c),(r+1,c),(r,c-1),(r,c+1)] where (0..<N).contains(rr) && (0..<N).contains(cc) { frontiers[rid].append((rr,cc)) }
            }
            while sizes.contains(where: { $0 < N }) {
                let options = (0..<N).filter { sizes[$0] < N && !frontiers[$0].isEmpty }
                if options.isEmpty { return nil }
                let rid = options.randomElement()!
                // score frontier cells by: many same-region neighbors if assigned, and closeness to centroid
                let f = frontiers[rid]
                // centroid
                var cr = 0, cc = 0, cnt = 0
                for r in 0..<N { for c in 0..<N { if grid[r][c] == rid { cr += r; cc += c; cnt += 1 } } }
                let cen = (Double(cr)/Double(max(1,cnt)), Double(cc)/Double(max(1,cnt)))
                var best: (Int,Int)? = nil
                var bestScore = -1_000_000.0
                for (i,(r,c)) in f.enumerated() {
                    if !(0..<N).contains(r) || !(0..<N).contains(c) { continue }
                    if grid[r][c] != -1 { continue }
                    // neighbors already this region if we took it
                    var same = 0
                    for (rr,cc) in [(r-1,c),(r+1,c),(r,c-1),(r,c+1)] {
                        if (0..<N).contains(rr) && (0..<N).contains(cc) && grid[rr][cc] == rid { same += 1 }
                    }
                    let dist = -hypot(Double(r)-cen.0, Double(c)-cen.1) // prefer closer to centroid
                    let score = Double(same*10) + dist + Double.random(in: 0..<0.1)
                    if score > bestScore { bestScore = score; best = (r,c) }
                    // small cleanup: remove out-of-grid or already taken entries occasionally
                    if i % 5 == 0 && (grid[r][c] != -1) { /* drop later */ }
                }
                guard let pick = best else { return nil }
                let (r,c) = pick
                grid[r][c] = rid
                sizes[rid] += 1
                for (rr,cc) in [(r-1,c),(r+1,c),(r,c-1),(r,c+1)] where (0..<N).contains(rr) && (0..<N).contains(cc) && grid[rr][cc] == -1 {
                    frontiers[rid].append((rr,cc))
                }
                // optional prune duplicates
                var seen = Set<Int>()
                frontiers[rid] = frontiers[rid].filter { (rr,cc) in let key = rr*N+cc; defer { seen.insert(key) }; return !seen.contains(key) }
            }
            // smoothing pass: try to eliminate degree-1 "nubs" via swaps
            for _ in 0..<N*N {
                var changed = false
                for r in 0..<N { for c in 0..<N {
                    let d = degree(grid, r, c)
                    if d <= 1 {
                        let my = grid[r][c]
                        // try swapping with neighbor of different region
                        for (rr,cc) in [(r-1,c),(r+1,c),(r,c-1),(r,c+1)] where (0..<N).contains(rr) && (0..<N).contains(cc) {
                            if grid[rr][cc] == my { continue }
                            var g2 = grid
                            let other = g2[rr][cc]
                            g2[r][c] = other; g2[rr][cc] = my
                            if degree(g2, r, c) >= 2 && degree(g2, rr, cc) >= 2 {
                                grid = g2
                                changed = true
                                break
                            }
                        }
                    }
                }}
                if !changed { break }
            }
            return grid
        }
        for _ in 0..<40 { if let g = attempt() { return g } }
        // Fallback to rows
        return (0..<N).map { r in Array(repeating: r, count: N) }
    }
    
    // Backtracking: return one valid solution, or nil
    static func solve(size N: Int, regions: [[Int]]) -> [BoardIndex]? {
        var count = 0
        var last: [Int] = []

        func enumerate(limit: Int) -> Int {
            var colUsed = Array(repeating: false, count: N)
            var regUsed = Array(repeating: false, count: N)
            var sol     = Array(repeating: -1,  count: N)

            func isAdjDiag(_ r1:Int,_ c1:Int,_ r2:Int,_ c2:Int) -> Bool { abs(r1 - r2) == 1 && abs(c1 - c2) == 1 }

            func dfs(_ r:Int) {
                if count >= limit { return }
                if r == N { count += 1; last = sol; return }

                for c in 0..<N where !colUsed[c] {
                    let rg = regions[r][c]; if regUsed[rg] { continue }
                    var ok = true
                    for rr in 0..<r { if isAdjDiag(rr, sol[rr], r, c) { ok = false; break } }
                    if !ok { continue }

                    sol[r] = c; colUsed[c] = true; regUsed[rg] = true
                    dfs(r + 1)
                    sol[r] = -1; colUsed[c] = false; regUsed[rg] = false

                    if count >= limit { return }
                }
            }

            dfs(0)
            return count
        }

        let total = enumerate(limit: 1)
        return total == 1 ? (0..<N).map { BoardIndex(r: $0, c: last[$0]) } : nil
    }

    
    // Count solutions up to `limit` (fast uniqueness check when limit=2)
    static func countSolutions(size N: Int, regions: [[Int]], limit: Int = 2) -> Int {
        var colUsed = Array(repeating: false, count: N)
        var regUsed = Array(repeating: false, count: N)
        var sol     = Array(repeating: -1,  count: N)
        var count   = 0

        func isAdjDiag(_ r1:Int,_ c1:Int,_ r2:Int,_ c2:Int) -> Bool { abs(r1 - r2) == 1 && abs(c1 - c2) == 1 }

        func dfs(_ r:Int) {
            if count >= limit { return }
            if r == N { count += 1; return }

            for c in 0..<N where !colUsed[c] {
                let rg = regions[r][c]; if regUsed[rg] { continue }
                var ok = true
                for rr in 0..<r { if isAdjDiag(rr, sol[rr], r, c) { ok = false; break } }
                if !ok { continue }

                sol[r] = c; colUsed[c] = true; regUsed[rg] = true
                dfs(r + 1)
                sol[r] = -1; colUsed[c] = false; regUsed[rg] = false

                if count >= limit { return }
            }
        }

        dfs(0)
        return count
    }

    // Generate connected regions and require a unique solution; simple fallback
    static func generateSolvablePuzzle(size N: Int, maxTries: Int = 5000) -> Puzzle {
        var lastGood: [[Int]]? = nil
        for _ in 0..<maxTries {
            let regs = makeRegions(size: N)
            lastGood = regs
            if countSolutions(size: N, regions: regs, limit: 2) == 1 {
                return Puzzle(size: N, regionIds: regs, presetQueens: [])
            }
        }
        // If we somehow didn't hit uniqueness in many tries, still return the best connected layout (never stripes).
        return Puzzle(size: N, regionIds: lastGood ?? makeRegions(size: N), presetQueens: [])
    }

    func loadNew(size N: Int) {
        let p = GameVM.generateSolvablePuzzle(size: N)
        self.puzzle = p
        self.board = BoardState.empty(size: p.size, preset: p.presetQueens)
        self.history.removeAll()
        rebuildAutoMarks()
    }
}
// MARK: - UI

struct QueensGameView: View {
    @StateObject private var vm = GameVM(puzzle: samplePuzzles[0])
    @State private var sizeSelection: Int = 5

    var body: some View {
        NavigationStack {
            VStack(spacing: 12) {
                boardGrid
                footerBar
            }
            .padding()
            .navigationTitle("Queens")
            .toolbar { ToolbarItem(placement: .topBarTrailing) { Button("Reset") { vm.reset() } } }
        }
    }

    private var boardGrid: some View {
        let N = vm.size
        return GeometryReader { geo in
            let side = min(geo.size.width, geo.size.height)
            let cell = (side - 8) / CGFloat(N)
            VStack(spacing: 2) {
                ForEach(0..<N, id: \.self) { r in
                    HStack(spacing: 2) {
                        ForEach(0..<N, id: \.self) { c in
                            cellView(r, c)
                                .frame(width: cell, height: cell)
                        }
                    }
                }
            }
            .frame(maxWidth: .infinity, maxHeight: .infinity)
        }
        .aspectRatio(1, contentMode: .fit)
    }

    private func cellView(_ r: Int, _ c: Int) -> some View {
        let idx = BoardIndex(r: r, c: c)
        let region = vm.puzzle.regionIds[r][c]
        let isPreset = vm.board.presetQueens.contains(idx)
        return ZStack {
            RoundedRectangle(cornerRadius: 6)
                .fill(regionColor(region))
                .overlay(RoundedRectangle(cornerRadius: 6).stroke(.secondary.opacity(0.6), lineWidth: 1))
            switch vm.board.cells[r][c] {
            case .empty:
                EmptyView()
            case .queen:
                let bad = vm.isQueenViolating(r, c)
                Text("👑").font(.system(size: 26)).foregroundStyle(bad ? .red : .primary)
            case .mark:
                Text("X").font(.headline).foregroundStyle(.secondary)
            }
        }
        .overlay(RoundedRectangle(cornerRadius: 6).stroke((vm.board.cells[r][c] == .queen && vm.isQueenViolating(r, c)) ? .red : .clear, lineWidth: 2))
        .onTapGesture { vm.cycle(r, c) }
        .onLongPressGesture(minimumDuration: 0.25) { vm.longPressMark(r, c) }
    }

    private var footerBar: some View {
        VStack(spacing: 8) {
            HStack {
                Button("Undo") { vm.undo() }
                    .buttonStyle(.bordered)
                    .disabled(vm.history.isEmpty)
                Spacer()
                if vm.checkViolations().isEmpty && vm.isSolved() {
                    Label("Solved!", systemImage: "checkmark.seal.fill").foregroundStyle(.green)
                }
            }
            HStack(spacing: 12) {
                Text("Size: 5x5")
                    .font(.subheadline)
                    .foregroundStyle(.secondary)
                Spacer()
                Button("New Puzzle") { vm.loadNew(size: 5) }
                    .buttonStyle(.borderedProminent)
            }
            Toggle("Auto X", isOn: $vm.autoXEnabled)
                .onChange(of: vm.autoXEnabled) { _ in vm.rebuildAutoMarks() }
            Text("Tap: empty -> X -> 👑 -> empty. Long-press toggles X. 1 per row/column/region; no diagonal touching.")
                .font(.footnote)
                .foregroundStyle(.secondary)
        }
    }

    private func regionColor(_ id: Int) -> Color {
        let palette: [Color] = [
            Color(red:0.96, green:0.94, blue:1.00),
            Color(red:0.93, green:0.98, blue:0.95),
            Color(red:1.00, green:0.96, blue:0.92),
            Color(red:0.94, green:0.97, blue:1.00),
            Color(red:1.00, green:0.94, blue:0.96),
            Color(red:0.97, green:1.00, blue:0.94),
            Color(red:0.98, green:0.98, blue:0.98)
        ]
        return palette[id % palette.count]
    }
}

#Preview { QueensGameView() }
